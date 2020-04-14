import           Development.Shake.FilePath     ( (</>) )
import           Fpm                            ( Arguments(..)
                                                , Command(..)
                                                , start
                                                )
import           System.Directory               ( withCurrentDirectory )

main :: IO ()
main = do
  testHelloWorld
  testHelloComplex

testHelloWorld :: IO ()
testHelloWorld =
  withCurrentDirectory "hello_world" $ start $ Arguments Run False

testHelloComplex :: IO ()
testHelloComplex =
  withCurrentDirectory "hello_complex" $ start $ Arguments Test False
