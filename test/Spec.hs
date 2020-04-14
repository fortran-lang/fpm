import           Development.Shake.FilePath     ( (</>) )
import           Fpm                            ( Arguments(..)
                                                , Command(..)
                                                , start
                                                )
import           System.Directory               ( withCurrentDirectory )

main :: IO ()
main = do
  testExampleProject

testExampleProject :: IO ()
testExampleProject =
  withCurrentDirectory "example_project" $ start $ Arguments Build False

testHelloWorld :: IO ()
testHelloWorld =
  withCurrentDirectory "hello_world" $ start $ Arguments Run False
