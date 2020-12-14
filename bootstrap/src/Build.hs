{-# LANGUAGE MultiWayIf #-}
module Build
  ( CompilerSettings(..)
  , buildLibrary
  , buildProgram
  , buildWithScript
  )
where

import           BuildModel                     ( AvailableModule(..)
                                                , CompileTimeInfo(..)
                                                , RawSource(..)
                                                , Source(..)
                                                , constructCompileTimeInfo
                                                , getAllObjectFiles
                                                , getAvailableModules
                                                , getSourceFileName
                                                , processRawSource
                                                )
import           Data.List                      ( intercalate
                                                , isSuffixOf
                                                )
import           Data.List.Utils                ( replace )
import           Development.Shake              ( FilePattern
                                                , Change(ChangeModtimeAndDigest)
                                                , cmd
                                                , getDirectoryFilesIO
                                                , liftIO
                                                , need
                                                , progressSimple
                                                , shake
                                                , shakeChange
                                                , shakeColor
                                                , shakeFiles
                                                , shakeOptions
                                                , shakeProgress
                                                , shakeThreads
                                                , want
                                                , (<//>)
                                                , (%>)
                                                , (&?>)
                                                )
import           Development.Shake.FilePath     ( exe
                                                , splitDirectories
                                                , (</>)
                                                , (<.>)
                                                )
import           System.Environment             ( setEnv )
import           System.FilePath                ( takeBaseName )
import           System.Process                 ( system )
import           System.Directory               ( createDirectoryIfMissing
                                                , makeAbsolute
                                                , withCurrentDirectory
                                                )

data CompilerSettings = CompilerSettings {
      compilerSettingsCompiler :: FilePath
    , compilerSettingsFlags :: [String]
    , compilerSettingsModuleFlag :: String
    , compilerSettingsIncludeFlag :: String
}

buildProgram
  :: FilePath
  -> [FilePath]
  -> [FilePattern]
  -> FilePath
  -> CompilerSettings
  -> String
  -> FilePath
  -> [FilePath]
  -> IO ()
buildProgram programDirectory' libraryDirectories sourceExtensions buildDirectory' (CompilerSettings { compilerSettingsCompiler = compiler, compilerSettingsFlags = flags, compilerSettingsModuleFlag = moduleFlag, compilerSettingsIncludeFlag = includeFlag }) programName programSource archives
  = do
    libraryModules <- findAvailableModules libraryDirectories
    let programDirectory = foldl1 (</>) (splitDirectories programDirectory')
    let buildDirectory   = foldl1 (</>) (splitDirectories buildDirectory')
    let includeFlags     = (includeFlag ++ buildDirectory) : map (includeFlag ++) libraryDirectories
    sourceFiles <- getDirectoriesFiles [programDirectory] sourceExtensions
    rawSources  <- mapM sourceFileToRawSource sourceFiles
    let sources' = map processRawSource rawSources
    let isThisProgramOrNotProgram p@(Program{}) =
          programSourceFileName p == programDirectory </> programSource
        isThisProgramOrNotProgram _ = True
    let sources          = filter isThisProgramOrNotProgram sources'
    let availableModules = (getAvailableModules sources buildDirectory) ++ libraryModules
    let compileTimeInfo = map
          (\s -> constructCompileTimeInfo s availableModules buildDirectory)
          sources
    let objectFiles = getAllObjectFiles buildDirectory sources
    shake shakeOptions { shakeFiles    = buildDirectory
                       , shakeChange   = ChangeModtimeAndDigest
                       , shakeColor    = True
                       , shakeThreads  = 0
                       , shakeProgress = progressSimple
                       }
      $ do
          let infoToRule cti =
                let obj                = compileTimeInfoObjectFileProduced cti
                    other              = compileTimeInfoOtherFilesProduced cti
                    directDependencies = compileTimeInfoDirectDependencies cti
                    sourceFile         = compileTimeInfoSourceFileName cti
                    fileMatcher f =
                        let realf = foldl1 (</>) (splitDirectories f)
                        in  if realf == obj || realf `elem` other
                              then Just (obj : other)
                              else Nothing
                in  fileMatcher &?> \(objectFile : _) -> do
                      need (sourceFile : directDependencies)
                      cmd compiler
                          ["-c", moduleFlag, buildDirectory]
                          includeFlags
                          flags
                          ["-o", objectFile, sourceFile]
          want [buildDirectory </> programName <.> exe]
          buildDirectory </> programName <.> exe %> \executable -> do
            need objectFiles
            need archives
            cmd compiler objectFiles archives ["-o", executable] flags
          mapM_ infoToRule compileTimeInfo

buildLibrary
  :: FilePath
  -> [FilePattern]
  -> FilePath
  -> CompilerSettings
  -> String
  -> [FilePath]
  -> IO (FilePath)
buildLibrary libraryDirectory sourceExtensions buildDirectory (CompilerSettings { compilerSettingsCompiler = compiler, compilerSettingsFlags = flags, compilerSettingsModuleFlag = moduleFlag, compilerSettingsIncludeFlag = includeFlag }) libraryName otherLibraryDirectories
  = do
    otherModules <- findAvailableModules otherLibraryDirectories
    let includeFlags = (includeFlag ++ buildDirectory) : map (includeFlag ++) otherLibraryDirectories
    sourceFiles <- getDirectoriesFiles [libraryDirectory] sourceExtensions
    rawSources  <- mapM sourceFileToRawSource sourceFiles
    let sources          = map processRawSource rawSources
    let availableModules = (getAvailableModules sources buildDirectory) ++ otherModules
    let compileTimeInfo = map
          (\s -> constructCompileTimeInfo s availableModules buildDirectory)
          sources
    let objectFiles = getAllObjectFiles buildDirectory sources
    let archiveFile = buildDirectory </> "lib" ++ libraryName <.> "a"
    shake shakeOptions { shakeFiles    = buildDirectory
                       , shakeChange   = ChangeModtimeAndDigest
                       , shakeColor    = True
                       , shakeThreads  = 0
                       , shakeProgress = progressSimple
                       }
      $ do
          let infoToRule cti =
                let obj                = compileTimeInfoObjectFileProduced cti
                    other              = compileTimeInfoOtherFilesProduced cti
                    directDependencies = compileTimeInfoDirectDependencies cti
                    sourceFile         = compileTimeInfoSourceFileName cti
                    fileMatcher f =
                        let realf = foldl1 (</>) (splitDirectories f)
                        in  if realf == obj || realf `elem` other
                              then Just (obj : other)
                              else Nothing
                in  fileMatcher &?> \(objectFile : _) -> do
                      need (sourceFile : directDependencies)
                      cmd compiler
                          ["-c", moduleFlag, buildDirectory]
                          includeFlags
                          flags
                          ["-o", objectFile, sourceFile]
          want [archiveFile]
          archiveFile %> \a -> do
            need objectFiles
            cmd "ar" ["rs"] a objectFiles
          mapM_ infoToRule compileTimeInfo
    return archiveFile

buildWithScript
  :: String
  -> FilePath
  -> FilePath
  -> CompilerSettings
  -> String
  -> [FilePath]
  -> IO (FilePath)
buildWithScript script projectDirectory buildDirectory (CompilerSettings { compilerSettingsCompiler = compiler, compilerSettingsFlags = flags, compilerSettingsModuleFlag = moduleFlag, compilerSettingsIncludeFlag = includeFlag }) libraryName otherLibraryDirectories
  = do
    absoluteBuildDirectory <- makeAbsolute buildDirectory
    createDirectoryIfMissing True absoluteBuildDirectory
    absoluteLibraryDirectories <- mapM makeAbsolute otherLibraryDirectories
    setEnv "FC"           compiler
    setEnv "FFLAGS"       (intercalate " " flags)
    setEnv "FINCLUDEFLAG" includeFlag
    setEnv "FMODUELFLAG"  moduleFlag
    setEnv "BUILD_DIR" $ unWindowsPath absoluteBuildDirectory
    setEnv "INCLUDE_DIRS"
           (intercalate " " (map unWindowsPath absoluteLibraryDirectories))
    let archiveFile =
          (unWindowsPath absoluteBuildDirectory)
            ++  "/lib"
            ++  libraryName
            <.> "a"
    withCurrentDirectory
      projectDirectory
      if
        | isMakefile script -> system
          ("make -f " ++ script ++ " " ++ archiveFile)
        | otherwise -> system (script ++ " " ++ archiveFile)
    return archiveFile

-- A little wrapper around getDirectoryFiles so we can get files from multiple directories
getDirectoriesFiles :: [FilePath] -> [FilePattern] -> IO [FilePath]
getDirectoriesFiles dirs exts = getDirectoryFilesIO "" newPatterns
 where
  newPatterns = concatMap appendExts dirs
  appendExts dir = map ((dir <//> "*") ++) exts

sourceFileToRawSource :: FilePath -> IO RawSource
sourceFileToRawSource sourceFile = do
  contents <- readFile sourceFile
  return $ RawSource sourceFile contents

isMakefile :: String -> Bool
isMakefile script | script == "Makefile"      = True
                  | script == "makefile"      = True
                  | ".mk" `isSuffixOf` script = True
                  | otherwise                 = False

unWindowsPath :: String -> String
unWindowsPath = changeSeparators . removeDriveLetter

removeDriveLetter :: String -> String
removeDriveLetter path | ':' `elem` path = (tail . dropWhile (/= ':')) path
                       | otherwise       = path

changeSeparators :: String -> String
changeSeparators = replace "\\" "/"

findAvailableModules :: [FilePath] -> IO [AvailableModule]
findAvailableModules directories = do
    moduleFiles <- getDirectoriesFiles directories ["*.mod"]
    let availableModules = map (\mf -> AvailableModule { availableModuleName = takeBaseName mf, availableModuleFile = mf }) moduleFiles
    return availableModules
