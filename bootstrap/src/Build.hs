{-# LANGUAGE MultiWayIf #-}
module Build
  ( buildLibrary
  , buildProgram
  , buildWithScript
  )
where

import           BuildModel                     ( CompileTimeInfo(..)
                                                , RawSource(..)
                                                , Source(..)
                                                , constructCompileTimeInfo
                                                , getAllObjectFiles
                                                , getAvailableModules
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
import           System.Process                 ( system )
import           System.Directory               ( createDirectoryIfMissing
                                                , makeAbsolute
                                                , withCurrentDirectory
                                                )

buildProgram
  :: FilePath
  -> [FilePath]
  -> [FilePattern]
  -> FilePath
  -> FilePath
  -> [String]
  -> String
  -> FilePath
  -> [FilePath]
  -> IO ()
buildProgram programDirectory libraryDirectories sourceExtensions buildDirectory compiler flags programName programSource archives
  = do
    let includeFlags = map ("-I" ++) libraryDirectories
    sourceFiles <- getDirectoriesFiles [programDirectory] sourceExtensions
    rawSources  <- mapM sourceFileToRawSource sourceFiles
    let sources' = map processRawSource rawSources
    let isThisProgramOrNotProgram p@(Program{}) =
          programSourceFileName p == programDirectory </> programSource
        isThisProgramOrNotProgram _ = True
    let sources          = filter isThisProgramOrNotProgram sources'
    let availableModules = getAvailableModules sources
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
                          ["-c", "-J" ++ buildDirectory]
                          includeFlags
                          flags
                          ["-o", objectFile, sourceFile]
          want [buildDirectory </> programName <.> exe]
          buildDirectory </> programName <.> exe %> \executable -> do
            liftIO $ print objectFiles
            need objectFiles
            cmd compiler objectFiles archives ["-o", executable] flags
          mapM_ infoToRule compileTimeInfo

buildLibrary
  :: FilePath
  -> [FilePattern]
  -> FilePath
  -> FilePath
  -> [String]
  -> String
  -> [FilePath]
  -> IO (FilePath)
buildLibrary libraryDirectory sourceExtensions buildDirectory compiler flags libraryName otherLibraryDirectories
  = do
    let includeFlags = map ("-I" ++) otherLibraryDirectories
    sourceFiles <- getDirectoriesFiles [libraryDirectory] sourceExtensions
    rawSources  <- mapM sourceFileToRawSource sourceFiles
    let sources          = map processRawSource rawSources
    let availableModules = getAvailableModules sources
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
                          ["-c", "-J" ++ buildDirectory]
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
  -> FilePath
  -> [String]
  -> String
  -> [FilePath]
  -> IO (FilePath)
buildWithScript script projectDirectory buildDirectory compiler flags libraryName otherLibraryDirectories
  = do
    absoluteBuildDirectory <- makeAbsolute buildDirectory
    createDirectoryIfMissing True absoluteBuildDirectory
    absoluteLibraryDirectories <- mapM makeAbsolute otherLibraryDirectories
    setEnv "FC"     compiler
    setEnv "FFLAGS" (intercalate " " flags)
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
