module ModuleToCompileInfoTest
  ( test
  )
where

import           BuildModel                     ( CompileTimeInfo(..)
                                                , Source(..)
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
  "a module and available modules"
  (exampleModule, availableModules)
  [ whenTransformed
      "its compileTimeInfo is determined"
      doCompileTimeTransformation
      [ then' "it stil knows the original source file"     checkSourceFileName
      , then' "it knows what object file will be produced" checkObjectFileName
      , then' "the mod and smod files are also produced" checkOtherFilesProduced
      , then' "the direct dependencies are only the available modules used"
              checkDirectDependencies
      ]
  ]

exampleModule :: Source
exampleModule = Module
  { moduleSourceFileName = moduleSourceFileName'
  , moduleObjectFileName = \bd -> bd </> "some_file_somewhere.f90.o"
  , moduleModulesUsed    = ["module1", "module2", "module3"]
  , moduleName           = "some_module"
  , moduleProducesSmod   = True
  }

moduleSourceFileName' :: FilePath
moduleSourceFileName' = "some" </> "file" </> "somewhere.f90"

availableModules :: [String]
availableModules = ["module1", "module3"]

doCompileTimeTransformation :: (Source, [String]) -> CompileTimeInfo
doCompileTimeTransformation (programSource, otherSources) =
  constructCompileTimeInfo programSource otherSources "build_dir"

checkSourceFileName :: CompileTimeInfo -> Result
checkSourceFileName cti =
  assertEquals moduleSourceFileName' (compileTimeInfoSourceFileName cti)

checkObjectFileName :: CompileTimeInfo -> Result
checkObjectFileName cti = assertEquals
  ("build_dir" </> "some_file_somewhere.f90.o")
  (compileTimeInfoObjectFileProduced cti)

checkOtherFilesProduced :: CompileTimeInfo -> Result
checkOtherFilesProduced cti = assertEquals
  ["build_dir" </> "some_module.mod", "build_dir" </> "some_module.smod"]
  (compileTimeInfoOtherFilesProduced cti)

checkDirectDependencies :: CompileTimeInfo -> Result
checkDirectDependencies cti = assertEquals
  ["build_dir" </> "module1.mod", "build_dir" </> "module3.mod"]
  (compileTimeInfoDirectDependencies cti)
