module Build
    ( buildLibrary
    , buildPrograms
    )
where

import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( isAsciiLower
                                                , isDigit
                                                , toLower
                                                )
import           Data.List                      ( intercalate )
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
import           System.FilePath                ( splitDirectories )
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

buildPrograms
    :: FilePath
    -> [FilePath]
    -> [FilePattern]
    -> FilePath
    -> FilePath
    -> [String]
    -> IO ()
buildPrograms programDirectory libraryDirectories sourceExtensions buildDirectory compiler flags
    = do
        sourceFiles <- getDirectoriesFiles [programDirectory] sourceExtensions
        let sourceFileLookupMap = createSourceFileLookupMap
                buildDirectory
                programDirectory
                sourceFiles
        libraryModuleMaps <- mapM getLibraryModuleMap libraryDirectories
        let libraryModuleMap = foldl Map.union Map.empty libraryModuleMaps
        let includeFlags     = map ("-I" ++) libraryDirectories
        archives <- getDirectoriesFiles libraryDirectories [".a"]
        let executables = map
                (sourceFileToExecutable buildDirectory programDirectory)
                sourceFiles
        shake shakeOptions { shakeFiles    = buildDirectory
                           , shakeChange   = ChangeModtimeAndDigest
                           , shakeColor    = True
                           , shakeThreads  = 0
                           , shakeProgress = progressSimple
                           }
            $ do
                  buildDirectory </> "*" <.> "o" %> \objectFile -> do
                      let realObjectFile = foldl (</>) "" $ splitDirectories objectFile
                      let sourceFile = fromMaybe
                              undefined
                              (Map.lookup realObjectFile sourceFileLookupMap
                              )
                      need [sourceFile]
                      modulesUsed <- liftIO $ getModulesUsed sourceFile
                      let
                          moduleFilesNeeded = mapMaybe
                              (`Map.lookup` libraryModuleMap)
                              modulesUsed
                      need moduleFilesNeeded
                      cmd compiler
                          ["-c"]
                          includeFlags
                          flags
                          ["-o", objectFile, sourceFile]
                  (`elem` executables) ?> \exe -> do
                      let objectFile = map toLower exe -<.> "o"
                      need [objectFile]
                      need archives
                      cmd compiler objectFile archives ["-o", exe] flags
                  want executables

buildLibrary
    :: FilePath
    -> [FilePattern]
    -> FilePath
    -> FilePath
    -> [String]
    -> String
    -> IO ()
buildLibrary libraryDirectory sourceExtensions buildDirectory compiler flags libraryName
    = do
        sourceFiles <- getDirectoriesFiles [libraryDirectory] sourceExtensions
        let sourceFileLookupMap = createSourceFileLookupMap
                buildDirectory
                libraryDirectory
                sourceFiles
        let moduleLookupMap = createModuleLookupMap buildDirectory
                                                    libraryDirectory
                                                    sourceFiles
        let archiveFile = buildDirectory </> libraryName <.> "a"
        shake shakeOptions { shakeFiles    = buildDirectory
                           , shakeChange   = ChangeModtimeAndDigest
                           , shakeColor    = True
                           , shakeThreads  = 0
                           , shakeProgress = progressSimple
                           }
            $ do
                  map (\ext -> buildDirectory </> "*" <.> ext) ["o", "mod"]
                      &%> \[objectFile, moduleFile] -> do
                              let realObjectFile = foldl (</>) "" $ splitDirectories objectFile
                              let sourceFile = fromMaybe
                                      undefined
                                      (Map.lookup realObjectFile sourceFileLookupMap
                                      )
                              need [sourceFile]
                              modulesUsed <- liftIO $ getModulesUsed sourceFile
                              let
                                  moduleFilesNeeded = mapMaybe
                                      (`Map.lookup` moduleLookupMap)
                                      modulesUsed
                              need moduleFilesNeeded
                              cmd compiler
                                  ["-c", "-J" ++ buildDirectory]
                                  flags
                                  ["-o", objectFile, sourceFile]
                  archiveFile %> \a -> do
                      let objectFiles = Map.keys sourceFileLookupMap
                      need objectFiles
                      cmd "ar" ["rs"] a objectFiles
                  want [archiveFile]

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
    buildDirectory
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
