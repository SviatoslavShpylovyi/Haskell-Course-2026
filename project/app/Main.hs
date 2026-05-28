module Main where

import Parser

main :: IO ()
main = do
  let input =
        unlines
          [ "// Simple dataflow pipeline"
          , "source input {"
          , "  value: 10"
          , "}"
          , ""
          , "transform double from input"
          , ""
          , "sink output from double"
          ]

  case parseProgram input of
    Nothing -> putStrLn "Parse error"
    Just program -> do
      putStrLn "Parsing successful!"
      print program