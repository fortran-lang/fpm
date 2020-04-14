import           Development.Shake.FilePath     ( (</>) )
import           Fpm                            ( Arguments(..)
                                                , Command(..)
                                                , start
                                                )
import           System.Directory               ( withCurrentDirectory )

main :: IO ()
main = do
  testExampleProject
  testHelloWorld
  testHelloComplex

testExampleProject :: IO ()
testExampleProject =
  withCurrentDirectory "example_project" $ start $ Arguments Build False

testHelloWorld :: IO ()
testHelloWorld =
  withCurrentDirectory "hello_world" $ start $ Arguments Run False

testHelloComplex :: IO ()
testHelloComplex =
  withCurrentDirectory "hello_complex" $ start $ Arguments Test False
