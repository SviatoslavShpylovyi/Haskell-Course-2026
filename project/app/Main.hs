module Main where

import Executor
import Parser

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError)


main :: IO ()
main = do
  args<-getArgs

  case args of 
    [filePath] -> runFile filePath
    [] -> do
      hPutStrLn stderr "Usage dataflow-project <file-path>"
      exitFailure
    _ -> do
      hPutStrLn stderr "Usage dataflow-project <file-path>"
      hPutStrLn stderr "Usage: Expected only one file path"
      exitFailure
runFile :: FilePath -> IO()
runFile filePath = do
  fileResult <-readPipelineFile filePath
  case fileResult of
    Left fileError -> do
      hPutStrLn stderr "File error"
      hPutStrLn stderr fileError
      exitFailure
    Right input ->
      runPipeline input

readPipelineFile :: FilePath -> IO (Either String String)
readPipelineFile filePath =
  (Right <$> readFile filePath)
    `catchIOError`
      \err -> pure (Left (show err))

runPipeline :: String -> IO ()
runPipeline input =
  case parseProgram input of
    Nothing -> do
      hPutStrLn stderr "Parse error"
      exitFailure

    Just program ->
      case executeProgram program of
        Left executionError -> do
          hPutStrLn stderr "Execution error:"
          hPutStrLn stderr (show executionError)
          exitFailure

        Right env -> do
          putStrLn "Parsing successful!"
          putStrLn "Validation successful!"
          putStrLn "Execution successful!"
          putStrLn "Final environment:"
          print env