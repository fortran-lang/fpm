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
                                                , assertEquals
                                                , assertThat
                                                , fail'
                                                , givenInput
                                                , then'
                                                , whenTransformed
                                                )
import           System.FilePath                ( (</>) )

test :: IO (Test ())
test = return $ givenInput
  "a module"
  exampleModule
  [ whenTransformed
      "processed to a source"
      processRawSource
      [ then' "it is a Module" checkIsModule
      , then' "its source file name is the same as the original"
              checkModuleSourceFileName
      ]
  ]

exampleModule :: RawSource
exampleModule =
  RawSource moduleSourceFileName' $ unlines ["module some_module", "end module"]

moduleSourceFileName' :: String
moduleSourceFileName' = "some" </> "file" </> "somewhere.f90"

checkIsModule :: Source -> Result
checkIsModule Module{} = assertThat True
checkIsModule _        = assertThat False

checkModuleSourceFileName :: Source -> Result
checkModuleSourceFileName m@(Module{}) =
  assertEquals moduleSourceFileName' $ moduleSourceFileName m
checkModuleSourceFileName _ = fail' "wasn't a Module"
