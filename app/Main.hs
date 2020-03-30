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
import           Toml                           ( TomlCodec
                                                , (.=)
                                                )
import qualified Toml

newtype Arguments = Arguments { command' :: Command }

data Settings = Settings {
      settingsCompiler :: !Text
    , settingsProjectName :: !Text
    , settingsDebugOptions :: ![Text]
    , settingsLibrary :: !Library }

data Library = Library { librarySourceDir :: !Text }

data Command = Run | Test | Build

main :: IO ()
main = do
  args        <- getArguments
  fpmContents <- TIO.readFile "fpm.toml"
  let settings = Toml.decode settingsCodec fpmContents
  case settings of
    Left  err      -> print err
    Right settings -> do
      app args settings

app :: Arguments -> Settings -> IO ()
app args settings = case command' args of
  Run   -> putStrLn "Run"
  Test  -> putStrLn "Test"
  Build -> build settings

build :: Settings -> IO ()
build settings = do
  putStrLn "Building"
  let compiler          = unpack $ settingsCompiler settings
  let projectName       = unpack $ settingsProjectName settings
  let flags             = map unpack $ settingsDebugOptions settings
  let librarySettings   = settingsLibrary settings
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

settingsCodec :: TomlCodec Settings
settingsCodec =
  Settings
    <$> Toml.text "compiler"
    .=  settingsCompiler
    <*> Toml.text "name"
    .=  settingsProjectName
    <*> Toml.arrayOf Toml._Text "debug-options"
    .=  settingsDebugOptions
    <*> Toml.table libraryCodec "library"
    .=  settingsLibrary

libraryCodec :: TomlCodec Library
libraryCodec = Library <$> Toml.text "source-dir" .= librarySourceDir
