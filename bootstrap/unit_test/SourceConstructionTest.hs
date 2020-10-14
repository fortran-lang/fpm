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
                                                , assertThat
                                                , givenInput
                                                , then'
                                                , whenTransformed
                                                )

test :: IO (Test ())
test = return $ givenInput
  "a program"
  exampleProgram
  [ whenTransformed "processed to a source"
                    processRawSource
                    [then' "it is a Program" checkIsProgram]
  ]

exampleProgram :: RawSource
exampleProgram = RawSource
  "some/file/somewhere.f90"
  $ unlines
  [ "program some_program"
  , "  implicit none"
  , "  print *, \"Hello, World!\""
  , "end program"
  ]

checkIsProgram :: Source -> Result
checkIsProgram s = assertThat $ case s of
  Program -> True
  _       -> False
