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
                                                , info
                                                , header
                                                , helper
                                                , progDesc
                                                , subparser
                                                )
import           System.Directory               ( doesDirectoryExist )
import           Toml                           ( TomlCodec
                                                , (.=)
                                                )
import qualified Toml

newtype Arguments = Arguments { command' :: Command }

data TomlSettings = TomlSettings {
      tomlSettingsCompiler :: !Text
    , tomlSettingsProjectName :: !Text
    , tomlSettingsLibrary :: !(Maybe Library) }

data AppSettings = AppSettings {
      appSettingsCompiler :: !Text
    , appSettingsProjectName :: !Text
    , appSettingsFlags :: ![Text]
    , appSettingsLibrary :: !(Maybe Library) }

data Library = Library { librarySourceDir :: !Text }

data Command = Run | Test | Build

main :: IO ()
main = do
  args        <- getArguments
  fpmContents <- TIO.readFile "fpm.toml"
  let tomlSettings = Toml.decode settingsCodec fpmContents
  case tomlSettings of
    Left  err           -> print err
    Right tomlSettings' -> do
      appSettings <- toml2AppSettings tomlSettings'
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
  case appSettingsLibrary settings of
    Just librarySettings -> do
      let librarySourceDir' = unpack $ librarySourceDir librarySettings
      buildLibrary librarySourceDir'
                   [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                   ("build" </> "library")
                   compiler
                   flags
                   projectName
                   []
      buildProgram "app"
                   ["build" </> "library"]
                   [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                   ("build" </> "app")
                   compiler
                   flags
                   projectName
                   "main.f90"
    Nothing -> do
      buildProgram "app"
                   []
                   [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                   ("build" </> "app")
                   compiler
                   flags
                   projectName
                   "main.f90"

getArguments :: IO Arguments
getArguments = execParser
  (info
    (arguments <**> helper)
    (fullDesc <> progDesc "Work with Fortran projects" <> header
      "fpm - A Fortran package manager and build system"
    )
  )

arguments :: Parser Arguments
arguments = subparser
  (  command "run"   (info runArguments (progDesc "Run the executable"))
  <> command "test"  (info testArguments (progDesc "Run the tests"))
  <> command "build" (info buildArguments (progDesc "Build the executable"))
  )

runArguments :: Parser Arguments
runArguments = pure $ Arguments Run

testArguments :: Parser Arguments
testArguments = pure $ Arguments Test

buildArguments :: Parser Arguments
buildArguments = pure $ Arguments Build

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

libraryCodec :: TomlCodec Library
libraryCodec = Library <$> Toml.text "source-dir" .= librarySourceDir

toml2AppSettings :: TomlSettings -> IO AppSettings
toml2AppSettings tomlSettings = do
  librarySettings <- getLibrarySettings $ tomlSettingsLibrary tomlSettings
  return AppSettings
    { appSettingsCompiler    = tomlSettingsCompiler tomlSettings
    , appSettingsProjectName = tomlSettingsProjectName tomlSettings
    , appSettingsFlags       = [ "-Wall"
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
    }

getLibrarySettings :: Maybe Library -> IO (Maybe Library)
getLibrarySettings maybeSettings = case maybeSettings of
  Just settings -> return maybeSettings
  Nothing       -> do
    defaultExists <- doesDirectoryExist "src"
    if defaultExists
      then return (Just (Library { librarySourceDir = "src" }))
      else return Nothing
