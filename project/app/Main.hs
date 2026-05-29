module Main where

import Parser
import Validator

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
          , "sink output from double"
          ]

  case parseProgram input of
    Nothing ->
      putStrLn "Parse error"

    Just program ->
      case validateProgram program of
        Left validationError -> do
          putStrLn "Validation error:"
          print validationError

        Right validProgram -> do
          putStrLn "Parsing successful!"
          putStrLn "Validation successful!"
          print validProgram