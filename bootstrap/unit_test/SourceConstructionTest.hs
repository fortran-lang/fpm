module SourceConstructionTest
  ( test
  )
where

import           Hedge                          ( Test
                                                , describe
                                                )

test :: IO (Test ())
test = return $ describe "something" []
