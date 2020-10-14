import           Development.Shake.FilePath     ( (</>) )
import           Fpm                            ( Arguments(..)
                                                , Command(..)
                                                , start
                                                )
import           System.Directory               ( withCurrentDirectory )

example_path = "test" </> "example_packages"

main :: IO ()
main = do
  testHelloWorld
  testHelloComplex
  testHelloFpm
  testCircular
  testWithMakefile
  testMakefileComplex

testHelloWorld :: IO ()
testHelloWorld =
  withCurrentDirectory (example_path </> "hello_world") $ start $ Arguments (Run "") False ""

testHelloComplex :: IO ()
testHelloComplex =
  withCurrentDirectory (example_path </> "hello_complex") $ start $ Arguments (Test "") False ""

testHelloFpm :: IO ()
testHelloFpm =
    withCurrentDirectory (example_path </> "hello_fpm") $ start $ Arguments (Run "") False ""

testCircular :: IO ()
testCircular =
    withCurrentDirectory (example_path </> "circular_example") $ start $ Arguments (Test "") False ""

testWithMakefile :: IO ()
testWithMakefile =
    withCurrentDirectory (example_path </> "with_makefile") $ start $ Arguments (Build) False ""

testMakefileComplex :: IO ()
testMakefileComplex =
    withCurrentDirectory (example_path </> "makefile_complex") $ start $ Arguments (Run "") False ""
