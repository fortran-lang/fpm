{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Build                          ( buildLibrary
                                                , buildProgram
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Data.Text.IO                  as TIO
import           Development.Shake              ( FilePattern
                                                , (<//>)
                                                , getDirectoryFilesIO
                                                )
import           Development.Shake.FilePath     ( (</>) )
import           Options.Applicative            ( Parser
                                                , (<**>)
                                                , command
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , progDesc
                                                , subparser
                                                , switch
                                                )
import           System.Directory               ( doesDirectoryExist )
import           Toml                           ( TomlCodec
                                                , (.=)
                                                )
import qualified Toml

data Arguments = Arguments { command' :: Command, release :: Bool }

data TomlSettings = TomlSettings {
      tomlSettingsCompiler :: !Text
    , tomlSettingsProjectName :: !Text
    , tomlSettingsLibrary :: !(Maybe Library)
    , tomlSettingsExecutables :: ![Executable]
}

data AppSettings = AppSettings {
      appSettingsCompiler :: !Text
    , appSettingsProjectName :: !Text
    , appSettingsFlags :: ![Text]
    , appSettingsLibrary :: !(Maybe Library)
    , appSettingsExecutables :: ![Executable]
}

data Library = Library { librarySourceDir :: !Text }

data Executable = Executable {
      executableSourceDir :: !Text
    , executableMainFile :: !Text
    , executableName :: !Text
} deriving Show

data Command = Run | Test | Build

main :: IO ()
main = do
  args        <- getArguments
  fpmContents <- TIO.readFile "fpm.toml"
  let tomlSettings = Toml.decode settingsCodec fpmContents
  case tomlSettings of
    Left  err           -> print err
    Right tomlSettings' -> do
      appSettings <- toml2AppSettings tomlSettings' (release args)
      app args appSettings

app :: Arguments -> AppSettings -> IO ()
app args settings = case command' args of
  Run   -> putStrLn "Run"
  Test  -> putStrLn "Test"
  Build -> build settings

build :: AppSettings -> IO ()
build settings = do
  putStrLn "Building"
  let compiler    = unpack $ appSettingsCompiler settings
  let projectName = unpack $ appSettingsProjectName settings
  let flags       = map unpack $ appSettingsFlags settings
  let executables = appSettingsExecutables settings
  executableDepends <- case appSettingsLibrary settings of
    Just librarySettings -> do
      let librarySourceDir' = unpack $ librarySourceDir librarySettings
      buildLibrary librarySourceDir'
                   [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                   ("build" </> "library")
                   compiler
                   flags
                   projectName
                   []
      return ["build" </> "library"]
    Nothing -> do
      return []
  mapM_
    (\Executable { executableSourceDir = sourceDir, executableMainFile = mainFile, executableName = name } ->
      do
        let sourceDir' = unpack sourceDir
        let name'      = unpack name
        let mainFile'  = unpack mainFile
        buildProgram sourceDir'
                     executableDepends
                     [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                     ("build" </> sourceDir')
                     compiler
                     flags
                     name'
                     mainFile'
    )
    executables

getArguments :: IO Arguments
getArguments = execParser
  (info
    (arguments <**> helper)
    (fullDesc <> progDesc "Work with Fortran projects" <> header
      "fpm - A Fortran package manager and build system"
    )
  )

arguments :: Parser Arguments
arguments =
  Arguments
    <$> subparser
          (  command "run"  (info runArguments (progDesc "Run the executable"))
          <> command "test" (info testArguments (progDesc "Run the tests"))
          <> command "build"
                     (info buildArguments (progDesc "Build the executable"))
          )
    <*> switch (long "release" <> help "Build in release mode")

runArguments :: Parser Command
runArguments = pure Run

testArguments :: Parser Command
testArguments = pure Test

buildArguments :: Parser Command
buildArguments = pure Build

getDirectoriesFiles :: [FilePath] -> [FilePattern] -> IO [FilePath]
getDirectoriesFiles dirs exts = getDirectoryFilesIO "" newPatterns
 where
  newPatterns = concatMap appendExts dirs
  appendExts dir = map ((dir <//> "*") ++) exts

settingsCodec :: TomlCodec TomlSettings
settingsCodec =
  TomlSettings
    <$> Toml.text "compiler"
    .=  tomlSettingsCompiler
    <*> Toml.text "name"
    .=  tomlSettingsProjectName
    <*> Toml.dioptional (Toml.table libraryCodec "library")
    .=  tomlSettingsLibrary
    <*> Toml.list executableCodec "executable"
    .=  tomlSettingsExecutables

libraryCodec :: TomlCodec Library
libraryCodec = Library <$> Toml.text "source-dir" .= librarySourceDir

executableCodec :: TomlCodec Executable
executableCodec =
  Executable
    <$> Toml.text "source-dir"
    .=  executableSourceDir
    <*> Toml.text "main"
    .=  executableMainFile
    <*> Toml.text "name"
    .=  executableName

toml2AppSettings :: TomlSettings -> Bool -> IO AppSettings
toml2AppSettings tomlSettings release = do
  librarySettings    <- getLibrarySettings $ tomlSettingsLibrary tomlSettings
  executableSettings <- getExecutableSettings
    $ tomlSettingsExecutables tomlSettings
  return AppSettings
    { appSettingsCompiler    = tomlSettingsCompiler tomlSettings
    , appSettingsProjectName = tomlSettingsProjectName tomlSettings
    , appSettingsFlags       = if release
                                 then
                                   [ "-Wall"
                                   , "-Wextra"
                                   , "-Wimplicit-interface"
                                   , "-Werror"
                                   , "-fPIC"
                                   , "-fmax-errors=1"
                                   , "-O3"
                                   , "-march=native"
                                   , "-ffast-math"
                                   , "-funroll-loops"
                                   ]
                                 else
                                   [ "-Wall"
                                   , "-Wextra"
                                   , "-Wimplicit-interface"
                                   , "-Werror"
                                   , "-fPIC"
                                   , "-fmax-errors=1"
                                   , "-g"
                                   , "-fbounds-check"
                                   , "-fcheck-array-temporaries"
                                   , "-fbacktrace"
                                   ]
    , appSettingsLibrary     = librarySettings
    , appSettingsExecutables = executableSettings
    }

getLibrarySettings :: Maybe Library -> IO (Maybe Library)
getLibrarySettings maybeSettings = case maybeSettings of
  Just settings -> return maybeSettings
  Nothing       -> do
    defaultExists <- doesDirectoryExist "src"
    if defaultExists
      then return (Just (Library { librarySourceDir = "src" }))
      else return Nothing

getExecutableSettings :: [Executable] -> IO [Executable]
getExecutableSettings []          = undefined
getExecutableSettings executables = return executables
