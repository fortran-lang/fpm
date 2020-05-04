{-# LANGUAGE OverloadedStrings #-}

module Fpm
  ( Arguments(..)
  , Command(..)
  , getArguments
  , start
  )
where

import           Build                          ( buildLibrary
                                                , buildProgram
                                                )
import qualified Data.Text.IO                  as TIO
import           Development.Shake              ( FilePattern
                                                , (<//>)
                                                , getDirectoryFilesIO
                                                )
import           Development.Shake.FilePath     ( (</>)
                                                , (<.>)
                                                , exe
                                                )
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
import           System.Directory               ( doesDirectoryExist
                                                , doesFileExist
                                                , makeAbsolute
                                                )
import           System.Process                 ( runCommand )
import           Toml                           ( TomlCodec
                                                , (.=)
                                                )
import qualified Toml

data Arguments = Arguments { command' :: Command, release :: Bool }

data TomlSettings = TomlSettings {
      tomlSettingsCompiler :: String
    , tomlSettingsProjectName :: String
    , tomlSettingsLibrary :: (Maybe Library)
    , tomlSettingsExecutables :: [Executable]
    , tomlSettingsTests :: [Executable]
}

data AppSettings = AppSettings {
      appSettingsCompiler :: String
    , appSettingsProjectName :: String
    , appSettingsBuildPrefix :: String
    , appSettingsFlags :: [String]
    , appSettingsLibrary :: (Maybe Library)
    , appSettingsExecutables :: [Executable]
    , appSettingsTests :: [Executable]
}

data Library = Library { librarySourceDir :: String }

data Executable = Executable {
      executableSourceDir :: String
    , executableMainFile :: String
    , executableName :: String
}

data Command = Run | Test | Build

start :: Arguments -> IO ()
start args = do
  fpmContents <- TIO.readFile "fpm.toml"
  let tomlSettings = Toml.decode settingsCodec fpmContents
  case tomlSettings of
    Left  err           -> print err
    Right tomlSettings' -> do
      appSettings <- toml2AppSettings tomlSettings' (release args)
      app args appSettings

app :: Arguments -> AppSettings -> IO ()
app args settings = case command' args of
  Build -> build settings
  Run   -> do
    build settings
    let buildPrefix = appSettingsBuildPrefix settings
    let
      executableNames = map
        (\Executable { executableSourceDir = sourceDir, executableMainFile = mainFile, executableName = name } ->
          sourceDir </> name
        )
        (appSettingsExecutables settings)
    let executables =
          map (buildPrefix </>) $ map (flip (<.>) exe) executableNames
    canonicalExecutables <- mapM makeAbsolute executables
    case canonicalExecutables of
      [] -> putStrLn "No Executables Found"
      _  -> mapM_ runCommand canonicalExecutables
  Test -> do
    build settings
    let buildPrefix = appSettingsBuildPrefix settings
    let
      executableNames = map
        (\Executable { executableSourceDir = sourceDir, executableMainFile = mainFile, executableName = name } ->
          sourceDir </> name
        )
        (appSettingsTests settings)
    let executables =
          map (buildPrefix </>) $ map (flip (<.>) exe) executableNames
    canonicalExecutables <- mapM makeAbsolute executables
    case canonicalExecutables of
      [] -> putStrLn "No Tests Found"
      _  -> mapM_ runCommand canonicalExecutables

build :: AppSettings -> IO ()
build settings = do
  let compiler    = appSettingsCompiler settings
  let projectName = appSettingsProjectName settings
  let buildPrefix = appSettingsBuildPrefix settings
  let flags       = appSettingsFlags settings
  let executables = appSettingsExecutables settings
  let tests       = appSettingsTests settings
  executableDepends <- case appSettingsLibrary settings of
    Just librarySettings -> do
      let librarySourceDir' = librarySourceDir librarySettings
      buildLibrary librarySourceDir'
                   [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                   (buildPrefix </> "library")
                   compiler
                   flags
                   projectName
                   []
      return [buildPrefix </> "library"]
    Nothing -> do
      return []
  mapM_
    (\Executable { executableSourceDir = sourceDir, executableMainFile = mainFile, executableName = name } ->
      do
        buildProgram sourceDir
                     executableDepends
                     [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                     (buildPrefix </> sourceDir)
                     compiler
                     flags
                     name
                     mainFile
    )
    executables
  mapM_
    (\Executable { executableSourceDir = sourceDir, executableMainFile = mainFile, executableName = name } ->
      do
        buildProgram sourceDir
                     executableDepends
                     [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                     (buildPrefix </> sourceDir)
                     compiler
                     flags
                     name
                     mainFile
    )
    tests

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
    <$> Toml.string "compiler"
    .=  tomlSettingsCompiler
    <*> Toml.string "name"
    .=  tomlSettingsProjectName
    <*> Toml.dioptional (Toml.table libraryCodec "library")
    .=  tomlSettingsLibrary
    <*> Toml.list executableCodec "executable"
    .=  tomlSettingsExecutables
    <*> Toml.list executableCodec "test"
    .=  tomlSettingsTests

libraryCodec :: TomlCodec Library
libraryCodec = Library <$> Toml.string "source-dir" .= librarySourceDir

executableCodec :: TomlCodec Executable
executableCodec =
  Executable
    <$> Toml.string "source-dir"
    .=  executableSourceDir
    <*> Toml.string "main"
    .=  executableMainFile
    <*> Toml.string "name"
    .=  executableName

toml2AppSettings :: TomlSettings -> Bool -> IO AppSettings
toml2AppSettings tomlSettings release = do
  let projectName = tomlSettingsProjectName tomlSettings
  librarySettings    <- getLibrarySettings $ tomlSettingsLibrary tomlSettings
  executableSettings <- getExecutableSettings
    (tomlSettingsExecutables tomlSettings)
    projectName
  testSettings <- getTestSettings $ tomlSettingsTests tomlSettings
  return AppSettings
    { appSettingsCompiler    = tomlSettingsCompiler tomlSettings
    , appSettingsProjectName = projectName
    , appSettingsBuildPrefix = "build"
                                 </> if release then "release" else "debug"
    , appSettingsFlags       = if release
                                 then
                                   [ "-Wall"
                                   , "-Wextra"
                                   , "-Wimplicit-interface"
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
                                   , "-fPIC"
                                   , "-fmax-errors=1"
                                   , "-g"
                                   , "-fbounds-check"
                                   , "-fcheck-array-temporaries"
                                   , "-fbacktrace"
                                   ]
    , appSettingsLibrary     = librarySettings
    , appSettingsExecutables = executableSettings
    , appSettingsTests       = testSettings
    }

getLibrarySettings :: Maybe Library -> IO (Maybe Library)
getLibrarySettings maybeSettings = case maybeSettings of
  Just settings -> return maybeSettings
  Nothing       -> do
    defaultExists <- doesDirectoryExist "src"
    if defaultExists
      then return (Just (Library { librarySourceDir = "src" }))
      else return Nothing

getExecutableSettings :: [Executable] -> String -> IO [Executable]
getExecutableSettings [] projectName = do
  defaultDirectoryExists <- doesDirectoryExist "app"
  if defaultDirectoryExists
    then do
      defaultMainExists <- doesFileExist ("app" </> "main.f90")
      if defaultMainExists
        then return
          [ Executable { executableSourceDir = "app"
                       , executableMainFile  = "main.f90"
                       , executableName      = projectName
                       }
          ]
        else return []
    else return []
getExecutableSettings executables _ = return executables

getTestSettings :: [Executable] -> IO [Executable]
getTestSettings [] = do
  defaultDirectoryExists <- doesDirectoryExist "test"
  if defaultDirectoryExists
    then do
      defaultMainExists <- doesFileExist ("test" </> "main.f90")
      if defaultMainExists
        then return
          [ Executable { executableSourceDir = "test"
                       , executableMainFile  = "main.f90"
                       , executableName      = "runTests"
                       }
          ]
        else return []
    else return []
getTestSettings tests = return tests
