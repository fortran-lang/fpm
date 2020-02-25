module Lib
    ( someFunc
    ) where

-- import Development.Shake (cmd)
import System.Process (callCommand)


someFunc :: IO ()
someFunc = do
    putStrLn "someFunc"
    a <- callCommand "gfortran test/test1.f90 -o test1"
    a <- callCommand "./test1"
    return ()
