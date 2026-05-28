module Main where

import AST
main :: IO ()
main = do
  let program =
        Program
          [ Node "input" Source [("value", NumVal 10)]
          , Node "double" Transform []
          , Node "output" Sink []
          ]
          [ Edge "input" "double"
          , Edge "double" "output"
          ]

  print program
