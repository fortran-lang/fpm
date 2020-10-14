module SourceConstructionTest
  ( test
  )
where

import           BuildModel                     ( RawSource(..)
                                                , Source(..)
                                                , processRawSource
                                                )
import           System.FilePath                ( (</>) )
import           Hedge                          ( Result
                                                , Test
                                                , assertEquals
                                                , assertThat
                                                , fail'
                                                , givenInput
                                                , then'
                                                , whenTransformed
                                                )

test :: IO (Test ())
test = return $ givenInput
  "a program"
  exampleProgram
  [ whenTransformed
      "processed to a source"
      processRawSource
      [ then' "it is a Program" checkIsProgram
      , then' "its source file name is the same as the original"
              checkProgramSourceFileName
      , then'
        "its object file name is the 'flattened' path of the source file with '.o' appended"
        checkProgramObjectFileName
      , then' "it knows what modules it uses directly" checkProgramModulesUsed
      ]
  ]

exampleProgram :: RawSource
exampleProgram = RawSource programSourceFileName' $ unlines
  [ "program some_program"
  , "  use module1"
  , "  USE MODULE2"
  , "  implicit none"
  , "  print *, \"Hello, World!\""
  , "end program"
  ]

programSourceFileName' :: String
programSourceFileName' = "some" </> "file" </> "somewhere.f90"

checkIsProgram :: Source -> Result
checkIsProgram Program{} = assertThat True
checkIsProgram _         = assertThat False

checkProgramSourceFileName :: Source -> Result
checkProgramSourceFileName p@(Program{}) =
  assertEquals programSourceFileName' $ programSourceFileName p
checkProgramSourceFileName _ = fail' "wasn't a Program"

checkProgramObjectFileName :: Source -> Result
checkProgramObjectFileName p@(Program{}) =
  assertEquals ("." </> "some_file_somewhere.f90.o")
    $ (programObjectFileName p) "."
checkProgramObjectFileName _ = fail' "wasn't a Program"

checkProgramModulesUsed :: Source -> Result
checkProgramModulesUsed p@(Program{}) = assertEquals ["module1", "module2"] $ programModulesUsed p
checkProgramModulesUsed _ = fail' "wasn't a Program"
