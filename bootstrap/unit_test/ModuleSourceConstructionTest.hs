module ModuleSourceConstructionTest
  ( test
  )
where

import           BuildModel                     ( RawSource(..)
                                                , Source(..)
                                                , processRawSource
                                                )
import           Hedge                          ( Result
                                                , Test
                                                , assertThat
                                                , givenInput
                                                , then'
                                                , whenTransformed
                                                )
import           System.FilePath                ( (</>) )

test :: IO (Test ())
test = return $ givenInput
  "a module"
  exampleModule
  [ whenTransformed "processed to a source"
                    processRawSource
                    [then' "it is a Module" checkIsModule]
  ]

exampleModule :: RawSource
exampleModule =
  RawSource moduleSourceFileName' $ unlines ["module some_module", "end module"]

moduleSourceFileName' :: String
moduleSourceFileName' = "some" </> "file" </> "somewhere.f90"

checkIsModule :: Source -> Result
checkIsModule Module{} = assertThat True
checkIsModule _        = assertThat False
