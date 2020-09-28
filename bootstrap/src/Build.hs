{-# LANGUAGE MultiWayIf #-}
module Build
  ( buildLibrary
  , buildProgram
  , buildWithScript
  )
where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( filterM )
import           Data.Char                      ( isAsciiLower
                                                , isDigit
                                                , toLower
                                                )
import           Data.List                      ( intercalate
                                                , isSuffixOf
                                                )
import           Data.List.Utils                ( replace )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
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
                                                , (&%>)
                                                , (%>)
                                                , (?>)
                                                )
import           Development.Shake.FilePath     ( dropExtension
                                                , exe
                                                , makeRelative
                                                , (</>)
                                                , (<.>)
                                                , (-<.>)
                                                )
import           System.Directory               ( createDirectoryIfMissing
                                                , makeAbsolute
                                                , withCurrentDirectory
                                                )
import           System.Environment             ( setEnv )
import           System.FilePath                ( splitDirectories )
import           System.Process                 ( system )
import           Text.ParserCombinators.ReadP   ( ReadP
                                                , char
                                                , eof
                                                , many
                                                , many1
                                                , option
                                                , readP_to_S
                                                , satisfy
                                                , skipSpaces
                                                , string
                                                )

type ModuleName = String

data LineContents = ModuleUsed ModuleName | Other

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
    sourceFiles <- getDirectoriesFiles [programDirectory] sourceExtensions
    canonicalProgramSource <- makeAbsolute $ programDirectory </> programSource
    moduleSourceFiles <- filterM
      (\source -> do
        canonicalSource <- makeAbsolute source
        return $ canonicalProgramSource /= canonicalSource
      )
      sourceFiles
    let moduleObjectFiles = map
          (sourceFileToObjectFile buildDirectory programDirectory)
          moduleSourceFiles
    let sourceFileLookupMap = createSourceFileLookupMap buildDirectory
                                                        programDirectory
                                                        moduleSourceFiles
    let moduleLookupMap = createModuleLookupMap buildDirectory
                                                programDirectory
                                                moduleSourceFiles
    otherModuleMaps <- mapM getLibraryModuleMap libraryDirectories
    let allModuleMaps =
          moduleLookupMap `Map.union` foldl Map.union Map.empty otherModuleMaps
    let includeFlags = map ("-I" ++) libraryDirectories
    shake shakeOptions { shakeFiles    = buildDirectory
                       , shakeChange   = ChangeModtimeAndDigest
                       , shakeColor    = True
                       , shakeThreads  = 0
                       , shakeProgress = progressSimple
                       }
      $ do
          want [buildDirectory </> programName <.> exe]
          buildDirectory </> programName <.> exe %> \executable -> do
            let objectFile = sourceFileToObjectFile buildDirectory
                                                    programDirectory
                                                    programSource
            let allObjectFiles = objectFile : moduleObjectFiles
            need allObjectFiles
            need archives
            cmd compiler allObjectFiles archives ["-o", executable] flags
          buildDirectory </> (map toLower programSource) -<.> "o" %> \objectFile -> do
            let realObjectFile = foldl (</>) "" $ splitDirectories objectFile
            let sourceFile     = programDirectory </> programSource
            need [sourceFile]
            modulesUsed <- liftIO $ getModulesUsed sourceFile
            let moduleFilesNeeded =
                  mapMaybe (`Map.lookup` allModuleMaps) modulesUsed
            let includeFlags = map ("-I" ++) libraryDirectories
            need moduleFilesNeeded
            cmd compiler
                ["-c", "-J" ++ buildDirectory]
                includeFlags
                flags
                ["-o", objectFile, sourceFile]
          map (\ext -> buildDirectory </> "*" <.> ext) ["o", "mod"]
            &%> \[objectFile, moduleFile] -> do
                  let realObjectFile =
                        foldl (</>) "" $ splitDirectories objectFile
                  let sourceFile = fromMaybe
                        undefined
                        (Map.lookup realObjectFile sourceFileLookupMap)
                  need [sourceFile]
                  modulesUsed <- liftIO $ getModulesUsed sourceFile
                  let moduleFilesNeeded =
                        mapMaybe (`Map.lookup` allModuleMaps) modulesUsed
                  let includeFlags = map ("-I" ++) libraryDirectories
                  need moduleFilesNeeded
                  cmd compiler
                      ["-c", "-J" ++ buildDirectory]
                      includeFlags
                      flags
                      ["-o", objectFile, sourceFile]

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
    sourceFiles <- getDirectoriesFiles [libraryDirectory] sourceExtensions
    let sourceFileLookupMap =
          createSourceFileLookupMap buildDirectory libraryDirectory sourceFiles
    let moduleLookupMap =
          createModuleLookupMap buildDirectory libraryDirectory sourceFiles
    otherModuleMaps <- mapM getLibraryModuleMap otherLibraryDirectories
    let allModuleMaps =
          moduleLookupMap `Map.union` foldl Map.union Map.empty otherModuleMaps
    let archiveFile = buildDirectory </> "lib" ++ libraryName <.> "a"
    shake shakeOptions { shakeFiles    = buildDirectory
                       , shakeChange   = ChangeModtimeAndDigest
                       , shakeColor    = True
                       , shakeThreads  = 0
                       , shakeProgress = progressSimple
                       }
      $ do
          map (\ext -> buildDirectory </> "*" <.> ext) ["o", "mod"]
            &%> \[objectFile, moduleFile] -> do
                  let realObjectFile =
                        foldl (</>) "" $ splitDirectories objectFile
                  let sourceFile = fromMaybe
                        undefined
                        (Map.lookup realObjectFile sourceFileLookupMap)
                  need [sourceFile]
                  modulesUsed <- liftIO $ getModulesUsed sourceFile
                  let moduleFilesNeeded =
                        mapMaybe (`Map.lookup` allModuleMaps) modulesUsed
                  let includeFlags = map ("-I" ++) otherLibraryDirectories
                  need moduleFilesNeeded
                  cmd compiler
                      ["-c", "-J" ++ buildDirectory]
                      includeFlags
                      flags
                      ["-o", objectFile, sourceFile]
          archiveFile %> \a -> do
            let objectFiles = Map.keys sourceFileLookupMap
            need objectFiles
            cmd "ar" ["rs"] a objectFiles
          want [archiveFile]
    return archiveFile

-- A little wrapper around getDirectoryFiles so we can get files from multiple directories
getDirectoriesFiles :: [FilePath] -> [FilePattern] -> IO [FilePath]
getDirectoriesFiles dirs exts = getDirectoryFilesIO "" newPatterns
 where
  newPatterns = concatMap appendExts dirs
  appendExts dir = map ((dir <//> "*") ++) exts

getLibraryModuleMap :: FilePath -> IO (Map.Map ModuleName FilePath)
getLibraryModuleMap libraryDirectory = do
  moduleFiles <- getDirectoriesFiles [libraryDirectory] ["*.mod"]
  let moduleMap = foldl
        Map.union
        Map.empty
        (map (\m -> Map.singleton (moduleFileToModuleName m) m) moduleFiles)
  return moduleMap
 where
  moduleFileToModuleName moduleFile =
    map toLower $ dropExtension (makeRelative libraryDirectory moduleFile)

createSourceFileLookupMap
  :: FilePath -> FilePath -> [FilePath] -> Map.Map FilePath FilePath
createSourceFileLookupMap buildDirectory libraryDirectory sourceFiles = foldl
  Map.union
  Map.empty
  (map (createSourceToObjectMap buildDirectory libraryDirectory) sourceFiles)

createModuleLookupMap
  :: FilePath -> FilePath -> [FilePath] -> Map.Map ModuleName FilePath
createModuleLookupMap buildDirectory libraryDirectory sourceFiles = foldl
  Map.union
  Map.empty
  (map (createSourceToModuleMap buildDirectory libraryDirectory) sourceFiles)

createSourceToModuleMap
  :: FilePath -> FilePath -> FilePath -> Map.Map ModuleName FilePath
createSourceToModuleMap buildDirectory libraryDirectory sourceFile =
  Map.singleton
    (sourceFileToModuleName libraryDirectory sourceFile)
    (sourceFileToModFile buildDirectory libraryDirectory sourceFile)

sourceFileToModuleName :: FilePath -> FilePath -> ModuleName
sourceFileToModuleName libraryDirectory sourceFile =
  map toLower $ pathSeparatorsToUnderscores
    (dropExtension (makeRelative libraryDirectory sourceFile))

createSourceToObjectMap
  :: FilePath -> FilePath -> FilePath -> Map.Map FilePath FilePath
createSourceToObjectMap buildDirectory libraryDirectory sourceFile =
  Map.singleton
    (sourceFileToObjectFile buildDirectory libraryDirectory sourceFile)
    sourceFile

sourceFileToObjectFile :: FilePath -> FilePath -> FilePath -> FilePath
sourceFileToObjectFile buildDirectory libraryDirectory sourceFile =
  (foldl (</>) "" $ splitDirectories buildDirectory)
    </>  map
           toLower
           (pathSeparatorsToUnderscores
             (makeRelative libraryDirectory sourceFile)
           )
    -<.> "o"

sourceFileToExecutable :: FilePath -> FilePath -> FilePath -> FilePath
sourceFileToExecutable buildDirectory appDirectory sourceFile =
  buildDirectory
    </>  pathSeparatorsToUnderscores (makeRelative appDirectory sourceFile)
    -<.> exe

sourceFileToModFile :: FilePath -> FilePath -> FilePath -> FilePath
sourceFileToModFile buildDirectory libraryDirectory sourceFile =
  buildDirectory
    </>  map
           toLower
           (pathSeparatorsToUnderscores
             (makeRelative libraryDirectory sourceFile)
           )
    -<.> "mod"

pathSeparatorsToUnderscores :: FilePath -> FilePath
pathSeparatorsToUnderscores fileName =
  intercalate "_" (splitDirectories fileName)

getModulesUsed :: FilePath -> IO [ModuleName]
getModulesUsed sourceFile = do
  fileLines <- readFileLinesIO sourceFile
  let lineContents = map parseFortranLine fileLines
  return $ contentsToModuleNames lineContents

contentsToModuleNames :: [LineContents] -> [ModuleName]
contentsToModuleNames = mapMaybe contentToMaybeModuleName
 where
  contentToMaybeModuleName content = case content of
    ModuleUsed moduleName -> Just moduleName
    _                     -> Nothing

readFileLinesIO :: FilePath -> IO [String]
readFileLinesIO file = do
  contents <- readFile file
  return $ lines contents

parseFortranLine :: String -> LineContents
parseFortranLine line =
  let line'  = map toLower line
      result = readP_to_S doFortranLineParse line'
  in  getResult result
 where
  getResult (_ : (contents, _) : _) = contents
  getResult [(contents, _)        ] = contents
  getResult []                      = Other

doFortranLineParse :: ReadP LineContents
doFortranLineParse = option Other fortranUsefulContents

fortranUsefulContents :: ReadP LineContents
fortranUsefulContents = useStatement

useStatement :: ReadP LineContents
useStatement = do
  skipSpaces
  _ <- string "use"
  skipAtLeastOneWhiteSpace
  modName <- validIdentifier
  skipSpaceCommaOrEnd
  return $ ModuleUsed modName

skipAtLeastOneWhiteSpace :: ReadP ()
skipAtLeastOneWhiteSpace = do
  _ <- many1 whiteSpace
  return ()

skipSpaceOrEnd :: ReadP ()
skipSpaceOrEnd = eof <|> skipAtLeastOneWhiteSpace

skipSpaceCommaOrEnd :: ReadP ()
skipSpaceCommaOrEnd = eof <|> skipComma <|> skipAtLeastOneWhiteSpace

skipComma :: ReadP ()
skipComma = do
  _ <- char ','
  return ()

whiteSpace :: ReadP Char
whiteSpace = satisfy (`elem` " \t")

validIdentifier :: ReadP String
validIdentifier = do
  first <- validFirstCharacter
  rest  <- many validIdentifierCharacter
  return $ first : rest

validFirstCharacter :: ReadP Char
validFirstCharacter = alphabet

validIdentifierCharacter :: ReadP Char
validIdentifierCharacter = alphabet <|> digit <|> underscore

alphabet :: ReadP Char
alphabet = satisfy isAsciiLower

digit :: ReadP Char
digit = satisfy isDigit

underscore :: ReadP Char
underscore = char '_'

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
    setEnv
      "INCLUDE_DIRS"
      (intercalate " " (map unWindowsPath absoluteLibraryDirectories))
    let archiveFile =
          (unWindowsPath absoluteBuildDirectory)
            ++ "/lib"
            ++  libraryName
            <.> "a"
    withCurrentDirectory
      projectDirectory
      if
        | isMakefile script -> system
          ("make -f " ++ script ++ " " ++ archiveFile)
        | otherwise -> system (script ++ " " ++ archiveFile)
    return archiveFile

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
