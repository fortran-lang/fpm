import           Development.Shake.FilePath     ( (</>) )
import           System.Process                 ( callCommand )

main :: IO ()
main = do
  someFunc
  someFunc2

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"
  a <- callCommand "gfortran test/test1.f90 -o test1"
  a <- callCommand $ "." </> "test1"
  return ()

someFunc2 :: IO ()
someFunc2 = do
  putStrLn "example"
  a <- callCommand "cd example_project && stack run -- build"
  return ()
