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

data LineContents = ModuleUsed String | Other

data RawSource = RawSource {
    rawSourceFilename :: FilePath
  , rawSourceContents :: String
}

data Source = Program {
    programSourceFileName :: FilePath
  , programObjectFileName :: FilePath -> FilePath
  , programModulesUsed :: [String]
}

processRawSource :: RawSource -> Source
processRawSource rawSource =
  let sourceFileName = rawSourceFilename rawSource
  in  Program
        { programSourceFileName = sourceFileName
        , programObjectFileName = \buildDirectory ->
                                    buildDirectory
                                      </> (pathSeparatorsToUnderscores
                                            sourceFileName
                                          )
                                      <.> "o"
        , programModulesUsed    = getModulesUsed rawSource
        }

pathSeparatorsToUnderscores :: FilePath -> FilePath
pathSeparatorsToUnderscores fileName =
  intercalate "_" (splitDirectories fileName)

getModulesUsed :: RawSource -> [String]
getModulesUsed rawSource =
  let fileLines    = lines $ rawSourceContents rawSource
      lineContents = map parseFortranLine fileLines
  in  contentsToModuleNames lineContents

contentsToModuleNames :: [LineContents] -> [String]
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
