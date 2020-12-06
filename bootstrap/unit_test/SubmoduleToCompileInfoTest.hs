module SubmoduleToCompileInfoTest
  ( test
  )
where

import           BuildModel                     ( AvailableModule(..)
                                                , CompileTimeInfo(..)
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
  "a submodule and available modules"
  (exampleSubmodule, availableModules)
  [ whenTransformed
      "its compileTimeInfo is determined"
      doCompileTimeTransformation
      [ then' "it still knows the original source file"    checkSourceFileName
      , then' "it knows what object file will be produced" checkObjectFileName
      , then' "the smod file is also produced" checkOtherFilesProduced
      , then'
        "the direct dependencies are the parent smod and the available modules used"
        checkDirectDependencies
      ]
  ]

exampleSubmodule :: Source
exampleSubmodule = Submodule
  { submoduleSourceFileName = submoduleSourceFileName'
  , submoduleObjectFileName = \bd -> bd </> "some_file_somewhere.f90.o"
  , submoduleModulesUsed    = ["module1", "module2", "module3"]
  , submoduleBaseModuleName = "base_module"
  , submoduleParentName     = "base_module@parent"
  , submoduleName           = "some_submodule"
  }

submoduleSourceFileName' :: FilePath
submoduleSourceFileName' = "some" </> "file" </> "somewhere.f90"

availableModules :: [AvailableModule]
availableModules = [ AvailableModule {availableModuleName = "module1", availableModuleFile = "build_dir" </> "module1.mod"}
                   , AvailableModule {availableModuleName = "module3", availableModuleFile = "build_dir" </> "module3.mod"}
                   ]

doCompileTimeTransformation :: (Source, [AvailableModule]) -> CompileTimeInfo
doCompileTimeTransformation (programSource, otherSources) =
  constructCompileTimeInfo programSource otherSources "build_dir"

checkSourceFileName :: CompileTimeInfo -> Result
checkSourceFileName cti =
  assertEquals submoduleSourceFileName' (compileTimeInfoSourceFileName cti)

checkObjectFileName :: CompileTimeInfo -> Result
checkObjectFileName cti = assertEquals
  ("build_dir" </> "some_file_somewhere.f90.o")
  (compileTimeInfoObjectFileProduced cti)

checkOtherFilesProduced :: CompileTimeInfo -> Result
checkOtherFilesProduced cti = assertEquals
  ["build_dir" </> "base_module@some_submodule.smod"]
  (compileTimeInfoOtherFilesProduced cti)

checkDirectDependencies :: CompileTimeInfo -> Result
checkDirectDependencies cti = assertEquals
  [ "build_dir" </> "base_module@parent.smod"
  , "build_dir" </> "module1.mod"
  , "build_dir" </> "module3.mod"
  ]
  (compileTimeInfoDirectDependencies cti)
