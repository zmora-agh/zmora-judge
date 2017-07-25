module Models where

type Source = String

type Program = FilePath

data CompilerParam = Param String | OutputFile | SourceCode deriving (Show)

data CompilerConfig = CompilerConfig {
    compiler :: FilePath,
    args     :: [CompilerParam],
    stdin    :: CompilerParam
} deriving (Show)
