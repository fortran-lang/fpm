module BuildModel where

data RawSource = RawSource {
    rawSourceFilename :: FilePath
  , rawSourceContents :: String
}

data Source = Program { programSourceFileName :: String}

processRawSource :: RawSource -> Source
processRawSource rawSource = Program $ rawSourceFilename rawSource
