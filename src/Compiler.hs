module Compiler where

import Models

import System.Process
import System.IO
import System.FilePath
import System.Exit

t = Task 32 "#include <stdio.h> \n void main() { printf(\"szatan\"); }" C

r task = compile task "/tmp/zmora"

compile :: Task -> FilePath -> IO (Either String Program)
compile task path = do
    let config = getCompilerConfig $ language task
    result <- compile' config (source task) outputFile
    return $ case result of
        Nothing -> Right outputFile
        Just error -> Left error
    where outputFile = path </> "output"

compile' :: CompilerConfig -> Source -> FilePath -> IO (Maybe String)
compile' config src out = do
    let input = parseCompilerParam src out (Models.stdin config)
    args <- return $ parseCompilerParam src out <$> args config
    result <- readProcessWithExitCode (compiler config) args input
    return $ case result of
        (ExitSuccess, _, _) -> Nothing
        (_, _, error) -> Just error


parseCompilerParam :: Source -> FilePath -> CompilerParam -> String
parseCompilerParam _ _ (Param p) = p
parseCompilerParam src _ SourceCode = src
parseCompilerParam _ outPath OutputFile = outPath

getCompilerConfig :: Language -> CompilerConfig
getCompilerConfig C = CompilerConfig "gcc" ((Param <$> ["-xc", "-", "-o"]) ++ [OutputFile]) SourceCode
getCompilerConfig CPP = CompilerConfig "g++" ((Param <$> ["-xc++", "-", "-o"]) ++ [OutputFile]) SourceCode