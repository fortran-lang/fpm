module ProgramToCompileInfoTest
  ( test
  )
where

import           BuildModel                     ( Source(..)
                                                , CompileTimeInfo(..)
                                                , constructCompileTimeInfo
                                                )
import           Hedge                          ( Result
                                                , Test
                                                , assertEquals
                                                , givenInput
                                                , then'
                                                , whenTransformed
                                                )
import           System.FilePath                ( (</>) )

test :: IO (Test ())
test = return $ givenInput
  "a program and other sources"
  (exampleProgram, exampleSources)
  [ whenTransformed
      "its compileTimeInfo is determined"
      doCompileTimeTransformation
      [then' "it still knows the original source file" checkSourceFileName]
  ]

exampleProgram :: Source
exampleProgram = Program
  { programSourceFileName = programSourceFileName'
  , programObjectFileName = \bd -> bd </> "some_file_somewhere.f90.o"
  , programModulesUsed    = ["module1", "module2", "module3"]
  }

programSourceFileName' :: String
programSourceFileName' = "some" </> "file" </> "somewhere.f90"

exampleSources :: [Source]
exampleSources = []

doCompileTimeTransformation :: (Source, [Source]) -> CompileTimeInfo
doCompileTimeTransformation (programSource, otherSources) =
  constructCompileTimeInfo programSource otherSources "build_dir"

checkSourceFileName :: CompileTimeInfo -> Result
checkSourceFileName cti =
  assertEquals programSourceFileName' (compileTimeInfoSourceFileName cti)
