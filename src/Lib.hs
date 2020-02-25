module Lib
    ( someFunc
    ) where

-- import Development.Shake (cmd)
import System.Process (callCommand)


someFunc :: IO ()
someFunc = do
    putStrLn "someFunc"
    a <- callCommand "ls -l"
    return ()
