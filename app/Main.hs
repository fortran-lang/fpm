module Main where

import           Build                          ( buildLibrary
                                                , buildPrograms
                                                )
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

newtype Arguments = Arguments { command' :: Command }

data Command = Run | Test | Build

main :: IO ()
main = do
    args <- getArguments
    app args

app :: Arguments -> IO ()
app args = case command' args of
    Run   -> putStrLn "Run"
    Test  -> putStrLn "Test"
    Build -> build

build :: IO ()
build = do
    putStrLn "Building"
    buildLibrary "src"
                 [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                 ("build" </> "library")
                 "gfortran"
                 ["-g", "-Wall", "-Wextra", "-Werror", "-pedantic"]
                 "library"
    buildPrograms "app"
                  ["build" </> "library"]
                  [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
                  ("build" </> "app")
                  "gfortran"
                  ["-g", "-Wall", "-Wextra", "-Werror", "-pedantic"]

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
