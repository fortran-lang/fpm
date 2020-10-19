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
  "a submodule"
  exampleSubmodule
  [ whenTransformed
      "processed to a source"
      processRawSource
      [ then' "it is a Submodule" checkIsSubmodule
      , then' "its source file name is the same as the original"
              checkSubmoduleSourceFileName
      , then'
        "its object file name is the 'flattened' path of the source file with '.o' appeneded"
        checkSubmoduleObjectFileName
      , then' "it knows what modules it uses directly" checkSubmoduleModulesUsed
      , then' "it knows its name"                      checkSubmoduleName
      ]
  ]

exampleSubmodule :: RawSource
exampleSubmodule = RawSource submoduleSourceFileName' $ unlines
  [ "submodule (some_module:parent) child"
  , "  use module1"
  , "  USE MODULE2"
  , "  implicit none"
  , "end submodule"
  ]

submoduleSourceFileName' :: String
submoduleSourceFileName' = "some" </> "file" </> "somewhere.f90"

checkIsSubmodule :: Source -> Result
checkIsSubmodule Submodule{} = assertThat True
checkIsSubmodule _           = assertThat False

checkSubmoduleSourceFileName :: Source -> Result
checkSubmoduleSourceFileName s@(Submodule{}) =
  assertEquals submoduleSourceFileName' $ submoduleSourceFileName s
checkSubmoduleSourceFileName _ = fail' "wasn't a Submodule"

checkSubmoduleObjectFileName :: Source -> Result
checkSubmoduleObjectFileName s@(Submodule{}) =
  assertEquals ("." </> "some_file_somewhere.f90.o")
    $ (submoduleObjectFileName s) "."
checkSubmoduleObjectFileName _ = fail' "wasn't a Submodule"

checkSubmoduleModulesUsed :: Source -> Result
checkSubmoduleModulesUsed s@(Submodule{}) =
  assertEquals ["module1", "module2"] $ submoduleModulesUsed s
checkSubmoduleModulesUsed _ = fail' "wasn't a Submodule"

checkSubmoduleName :: Source -> Result
checkSubmoduleName s@(Submodule{}) =
  assertEquals "some_module@parent@child" $ submoduleName s
checkSubmoduleName _ = fail' "wasn't a Submodule"
