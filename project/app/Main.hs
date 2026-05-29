module Main where

import Executor
import Parser

main :: IO ()
main = do
  let input =
        unlines
          [ "source input {"
          , "  value: 10"
          , "}"
          , ""
          , "transform double from input"
          , ""
          , "transform increment from double"
          , ""
          , "sink output from increment"
          ]

  case parseProgram input of
    Nothing ->
      putStrLn "Parse error"

    Just program ->
      case executeProgram program of
        Left executionError -> do
          putStrLn "Execution error:"
          print executionError

        Right env -> do
          putStrLn "Parsing successful!"
          putStrLn "Validation successful!"
          putStrLn "Execution successful!"
          putStrLn "Final environment:"
          print env