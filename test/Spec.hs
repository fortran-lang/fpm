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
  testHelloFpm
  testCircular

testHelloWorld :: IO ()
testHelloWorld =
  withCurrentDirectory "hello_world" $ start $ Arguments Run False

testHelloComplex :: IO ()
testHelloComplex =
  withCurrentDirectory "hello_complex" $ start $ Arguments Test False

testHelloFpm :: IO ()
testHelloFpm =
    withCurrentDirectory "hello_fpm" $ start $ Arguments Run False

testCircular :: IO ()
testCircular =
    withCurrentDirectory "circular_example" $ start $ Arguments Test False
