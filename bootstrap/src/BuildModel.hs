module BuildModel where

import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( isAsciiLower
                                                , isDigit
                                                , toLower
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import           Data.List                      ( intercalate )
import           System.FilePath                ( (</>)
                                                , (<.>)
                                                , splitDirectories
                                                )
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

data LineContents =
    ProgramDeclaration
  | ModuleDeclaration String
  | ModuleUsed String
  | ModuleSubprogramDeclaration
  | SubmoduleDeclaration String
  | Other

data RawSource = RawSource {
    rawSourceFilename :: FilePath
  , rawSourceContents :: String
}

data Source =
  Program
    { programSourceFileName :: FilePath
    , programObjectFileName :: FilePath -> FilePath
    , programModulesUsed :: [String]
    }
  | Module
    { moduleSourceFileName :: FilePath
    , moduleObjectFileName :: FilePath -> FilePath
    , moduleModulesUsed :: [String]
    , moduleName :: String
    , moduleProducesSmod :: Bool
    }
  | Submodule
    { submoduleSourceFileName :: FilePath
    , submoduleObjectFileName :: FilePath -> FilePath
    , submoduleModulesUsed :: [String]
    , submoduleName :: String
    }

data CompileTimeInfo = CompileTimeInfo {
    compileTimeInfoSourceFileName :: FilePath
  , compileTimeInfoObjectFileProduced :: FilePath
  , compileTimeInfoOtherFilesProduced :: [FilePath]
}

processRawSource :: RawSource -> Source
processRawSource rawSource =
  let sourceFileName = rawSourceFilename rawSource
      parsedContents = parseContents rawSource
      objectFileName =
          \bd -> bd </> (pathSeparatorsToUnderscores sourceFileName) <.> "o"
      modulesUsed = getModulesUsed parsedContents
  in  if hasProgramDeclaration parsedContents
        then Program { programSourceFileName = sourceFileName
                     , programObjectFileName = objectFileName
                     , programModulesUsed    = modulesUsed
                     }
        else if hasModuleDeclaration parsedContents
          then Module
            { moduleSourceFileName = sourceFileName
            , moduleObjectFileName = objectFileName
            , moduleModulesUsed    = modulesUsed
            , moduleName           = getModuleName parsedContents
            , moduleProducesSmod = hasModuleSubprogramDeclaration parsedContents
            }
          else if hasSubmoduleDeclaration parsedContents
            then Submodule { submoduleSourceFileName = sourceFileName
                           , submoduleObjectFileName = objectFileName
                           , submoduleModulesUsed    = modulesUsed
                           , submoduleName = getSubmoduleName parsedContents
                           }
            else undefined

constructCompileTimeInfo :: Source -> [Source] -> FilePath -> CompileTimeInfo
constructCompileTimeInfo program@(Program{}) otherSources buildDirectory =
  CompileTimeInfo
    { compileTimeInfoSourceFileName     = programSourceFileName program
    , compileTimeInfoObjectFileProduced = (programObjectFileName program)
                                            buildDirectory
    , compileTimeInfoOtherFilesProduced = []
    }
constructCompileTimeInfo _ otherSources buildDirectory = undefined

pathSeparatorsToUnderscores :: FilePath -> FilePath
pathSeparatorsToUnderscores fileName =
  intercalate "_" (splitDirectories fileName)

parseContents :: RawSource -> [LineContents]
parseContents rawSource =
  let fileLines = lines $ rawSourceContents rawSource
  in  map parseFortranLine fileLines

hasProgramDeclaration :: [LineContents] -> Bool
hasProgramDeclaration parsedContents = case filter f parsedContents of
  x : _ -> True
  _     -> False
 where
  f lc = case lc of
    ProgramDeclaration -> True
    _                  -> False

hasModuleDeclaration :: [LineContents] -> Bool
hasModuleDeclaration parsedContents = case filter f parsedContents of
  x : _ -> True
  _     -> False
 where
  f lc = case lc of
    ModuleDeclaration{} -> True
    _                   -> False

hasSubmoduleDeclaration :: [LineContents] -> Bool
hasSubmoduleDeclaration parsedContents = case filter f parsedContents of
  x : _ -> True
  _     -> False
 where
  f lc = case lc of
    SubmoduleDeclaration{} -> True
    _                      -> False

hasModuleSubprogramDeclaration :: [LineContents] -> Bool
hasModuleSubprogramDeclaration parsedContents = case filter f parsedContents of
  x : _ -> True
  _     -> False
 where
  f lc = case lc of
    ModuleSubprogramDeclaration -> True
    _                           -> False

getModulesUsed :: [LineContents] -> [String]
getModulesUsed = mapMaybe contentToMaybeModuleName
 where
  contentToMaybeModuleName content = case content of
    ModuleUsed moduleName -> Just moduleName
    _                     -> Nothing

getModuleName :: [LineContents] -> String
getModuleName pc = head $ mapMaybe contentToMaybeModuleName pc
 where
  contentToMaybeModuleName content = case content of
    ModuleDeclaration moduleName -> Just moduleName
    _                            -> Nothing

getSubmoduleName :: [LineContents] -> String
getSubmoduleName pc = head $ mapMaybe contentToMaybeModuleName pc
 where
  contentToMaybeModuleName content = case content of
    SubmoduleDeclaration submoduleName -> Just submoduleName
    _ -> Nothing

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
fortranUsefulContents =
  programDeclaration
    <|> moduleSubprogramDeclaration
    <|> moduleDeclaration
    <|> submoduleDeclaration
    <|> useStatement

programDeclaration :: ReadP LineContents
programDeclaration = do
  skipSpaces
  _ <- string "program"
  skipAtLeastOneWhiteSpace
  _ <- validIdentifier
  return ProgramDeclaration

moduleDeclaration :: ReadP LineContents
moduleDeclaration = do
  skipSpaces
  _ <- string "module"
  skipAtLeastOneWhiteSpace
  moduleName <- validIdentifier
  skipSpaceCommentOrEnd
  return $ ModuleDeclaration moduleName

submoduleDeclaration :: ReadP LineContents
submoduleDeclaration = do
  skipSpaces
  _       <- string "submodule"
  parents <- submoduleParents
  skipSpaces
  name <- validIdentifier
  skipSpaceCommentOrEnd
  return $ SubmoduleDeclaration ((intercalate "@" parents) ++ "@" ++ name)

submoduleParents :: ReadP [String]
submoduleParents = do
  skipSpaces
  _ <- char '('
  skipSpaces
  firstParent      <- validIdentifier
  remainingParents <- many
    (do
      skipSpaces
      _ <- char ':'
      skipSpaces
      name <- validIdentifier
      return name
    )
  skipSpaces
  _ <- char ')'
  return $ firstParent : remainingParents

useStatement :: ReadP LineContents
useStatement = do
  skipSpaces
  _ <- string "use"
  skipAtLeastOneWhiteSpace
  modName <- validIdentifier
  skipSpaceCommaOrEnd
  return $ ModuleUsed modName

moduleSubprogramDeclaration :: ReadP LineContents
moduleSubprogramDeclaration = do
  skipAnything
  _ <- string "module"
  skipAtLeastOneWhiteSpace
  skipAnything
  _ <- string "function" <|> string "subroutine"
  skipAtLeastOneWhiteSpace
  return $ ModuleSubprogramDeclaration

skipAtLeastOneWhiteSpace :: ReadP ()
skipAtLeastOneWhiteSpace = do
  _ <- many1 whiteSpace
  return ()

skipSpaceOrEnd :: ReadP ()
skipSpaceOrEnd = eof <|> skipAtLeastOneWhiteSpace

skipSpaceCommaOrEnd :: ReadP ()
skipSpaceCommaOrEnd = eof <|> skipComma <|> skipAtLeastOneWhiteSpace

skipSpaceCommentOrEnd :: ReadP ()
skipSpaceCommentOrEnd = eof <|> skipComment <|> skipAtLeastOneWhiteSpace

skipComma :: ReadP ()
skipComma = do
  _ <- char ','
  return ()

skipComment :: ReadP ()
skipComment = do
  _ <- char '!'
  return ()

skipAnything :: ReadP ()
skipAnything = do
  _ <- many (satisfy (const True))
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
