module Main where

import           Options.Applicative

newtype Arguments = Arguments { command' :: Command }

data Command = Run | Test | Build

main :: IO ()
main = do
    args <- getArguments
    run args

run :: Arguments -> IO ()
run args = case command' args of
    Run   -> putStrLn "Run"
    Test  -> putStrLn "Test"
    Build -> putStrLn "Build"

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
