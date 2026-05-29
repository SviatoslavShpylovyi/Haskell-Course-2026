module Main where

-- import AST
-- import Parser
-- import Test.QuickCheck

-- -- Helper function for readable parser tests
-- shouldParseAs :: String -> Program -> Property
-- shouldParseAs input expected =
--   parseProgram input === Just expected

-- -- ============================================================
-- -- Fixed parser tests
-- -- ============================================================

-- prop_parseSimpleNumericPipeline :: Property
-- prop_parseSimpleNumericPipeline =
--   shouldParseAs input expected
--   where
--     input =
--       unlines
--         [ "source input {"
--         , "  value: 10"
--         , "}"
--         , ""
--         , "transform double from input"
--         , ""
--         , "sink output from double"
--         ]

--     expected =
--       Program
--         [ Node "input" Source [("value", NumVal 10)]
--         , Node "double" Transform []
--         , Node "output" Sink []
--         ]
--         [ Edge "input" "double"
--         , Edge "double" "output"
--         ]

-- prop_parseStringValue :: Property
-- prop_parseStringValue =
--   shouldParseAs input expected
--   where
--     input =
--       unlines
--         [ "source message {"
--         , "  value: \"hello\""
--         , "}"
--         , "sink output from message"
--         ]

--     expected =
--       Program
--         [ Node "message" Source [("value", StrVal "hello")]
--         , Node "output" Sink []
--         ]
--         [ Edge "message" "output" ]

-- prop_parseBoolValue :: Property
-- prop_parseBoolValue =
--   shouldParseAs input expected
--   where
--     input =
--       unlines
--         [ "source flag {"
--         , "  value: true"
--         , "}"
--         , "sink output from flag"
--         ]

--     expected =
--       Program
--         [ Node "flag" Source [("value", BoolVal True)]
--         , Node "output" Sink []
--         ]
--         [ Edge "flag" "output" ]

-- prop_parseListValue :: Property
-- prop_parseListValue =
--   shouldParseAs input expected
--   where
--     input =
--       unlines
--         [ "source numbers {"
--         , "  value: [1, 2, 3]"
--         , "}"
--         , "sink output from numbers"
--         ]

--     expected =
--       Program
--         [ Node "numbers" Source [("value", ListVal [NumVal 1, NumVal 2, NumVal 3])]
--         , Node "output" Sink []
--         ]
--         [ Edge "numbers" "output" ]

-- prop_parseComments :: Property
-- prop_parseComments =
--   shouldParseAs input expected
--   where
--     input =
--       unlines
--         [ "// line comment"
--         , "source input {"
--         , "  value: 10"
--         , "}"
--         , "/* block comment */"
--         , "sink output from input"
--         ]

--     expected =
--       Program
--         [ Node "input" Source [("value", NumVal 10)]
--         , Node "output" Sink []
--         ]
--         [ Edge "input" "output" ]

-- prop_rejectInvalidSyntax :: Property
-- prop_rejectInvalidSyntax =
--   parseProgram input === Nothing
--   where
--     input =
--       unlines
--         [ "source {"
--         , "  value: 10"
--         , "}"
--         ]

-- -- ============================================================
-- -- Generated QuickCheck tests
-- -- ============================================================

-- prop_parseGeneratedNumber :: Small Int -> Property
-- prop_parseGeneratedNumber (Small n) =
--   shouldParseAs input expected
--   where
--     input =
--       unlines
--         [ "source input {"
--         , "  value: " ++ show n
--         , "}"
--         ]

--     expected =
--       Program
--         [ Node "input" Source [("value", NumVal (fromIntegral n))]
--         ]
--         []

-- prop_parseGeneratedNodeNames :: Positive Int -> Positive Int -> Property
-- prop_parseGeneratedNodeNames (Positive x) (Positive y) =
--   shouldParseAs input expected
--   where
--     sourceName = "sourceNode" ++ show x
--     sinkName = "sinkNode" ++ show y

--     input =
--       unlines
--         [ "source " ++ sourceName ++ " {"
--         , "  value: 1"
--         , "}"
--         , "sink " ++ sinkName ++ " from " ++ sourceName
--         ]

--     expected =
--       Program
--         [ Node sourceName Source [("value", NumVal 1)]
--         , Node sinkName Sink []
--         ]
--         [ Edge sourceName sinkName ]

-- prop_parseGeneratedList :: [Small Int] -> Property
-- prop_parseGeneratedList smallValues =
--   shouldParseAs input expected
--   where
--     values = map getSmall smallValues

--     input =
--       unlines
--         [ "source numbers {"
--         , "  value: [" ++ joinWithComma (map show values) ++ "]"
--         , "}"
--         ]

--     expected =
--       Program
--         [ Node "numbers" Source [("value", ListVal (map (NumVal . fromIntegral) values))]
--         ]
--         []

-- joinWithComma :: [String] -> String
-- joinWithComma [] = ""
-- joinWithComma [x] = x
-- joinWithComma (x : xs) = x ++ ", " ++ joinWithComma xs

-- -- ============================================================
-- -- Test runner
-- -- ============================================================

main :: IO ()
main = do
   putStrLn "Parser tests"

--   quickCheck prop_parseSimpleNumericPipeline
--   quickCheck prop_parseStringValue
--   quickCheck prop_parseBoolValue
--   quickCheck prop_parseListValue
--   quickCheck prop_parseComments
--   quickCheck prop_rejectInvalidSyntax

--   quickCheck prop_parseGeneratedNumber
--   quickCheck prop_parseGeneratedNodeNames
--   quickCheck prop_parseGeneratedList