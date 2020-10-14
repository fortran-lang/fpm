module SourceConstructionTest
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
      ]
  ]

exampleProgram :: RawSource
exampleProgram = RawSource programSourceFileName' $ unlines
  [ "program some_program"
  , "  implicit none"
  , "  print *, \"Hello, World!\""
  , "end program"
  ]

programSourceFileName' :: String
programSourceFileName' = "some/file/somewhere.f90"

checkIsProgram :: Source -> Result
checkIsProgram s = assertThat $ case s of
  Program{} -> True
  _       -> False

checkProgramSourceFileName :: Source -> Result
checkProgramSourceFileName s = case s of
  p@(Program{}) ->
    assertEquals programSourceFileName' $ programSourceFileName p
  _ -> fail' "wasn't a Program"
