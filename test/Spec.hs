import           Development.Shake.FilePath     ( (</>) )
import           System.Directory               ( withCurrentDirectory )
import           System.Process                 ( callCommand )

main :: IO ()
main = do
  testExampleProject

testExampleProject :: IO ()
testExampleProject =
  withCurrentDirectory "example_project" $ callCommand "stack run -- build"
