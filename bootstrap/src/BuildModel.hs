module BuildModel where

import           Data.List                      ( intercalate )
import           System.FilePath                ( (</>)
                                                , (<.>)
                                                , splitDirectories
                                                )

data RawSource = RawSource {
    rawSourceFilename :: FilePath
  , rawSourceContents :: String
}

data Source = Program {
    programSourceFileName :: FilePath
  , programObjectFileName :: FilePath -> FilePath
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
        }

pathSeparatorsToUnderscores :: FilePath -> FilePath
pathSeparatorsToUnderscores fileName =
  intercalate "_" (splitDirectories fileName)
