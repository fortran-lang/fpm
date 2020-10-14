module BuildModel where

data RawSource = RawSource {
    rawSourceFilename :: FilePath
  , rawSourceContents :: String
}

data Source = Program

processRawSource :: RawSource -> Source
processRawSource _ = Program
