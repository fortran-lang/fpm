module SubmoduleSourceConstructionTest
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
  "a submodule"
  exampleSubmodule
  [ whenTransformed "processed to a source"
                    processRawSource
                    [then' "it is a Submodule" checkIsSubmodule]
  ]

exampleSubmodule :: RawSource
exampleSubmodule = RawSource submoduleSourceFileName'
  $ unlines ["submodule (some_module:parent) child", "end submodule"]

submoduleSourceFileName' :: String
submoduleSourceFileName' = "some" </> "file" </> "somewhere.f90"

checkIsSubmodule :: Source -> Result
checkIsSubmodule Submodule{} = assertThat True
checkIsSubmodule _           = assertThat False
