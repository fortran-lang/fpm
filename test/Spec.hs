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
