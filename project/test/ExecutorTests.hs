module ExecutorTests
  ( runExecutorTests
  ) where

import AST
import Errors
import Executor
import Parser

import qualified Data.Map.Strict as Map

import Test.QuickCheck


shouldExecuteAs :: Program -> Env -> Property
shouldExecuteAs program expectedEnv =
  executeProgram program === Right expectedEnv

shouldFailWith :: Program -> ExecutionError -> Property
shouldFailWith program expectedError =
  executeProgram program === Left expectedError

shouldParseAndExecuteAs :: String -> Env -> Property
shouldParseAndExecuteAs input expectedEnv =
  case parseProgram input of
    Nothing ->
      counterexample "Parser failed before execution" False

    Just program ->
      executeProgram program === Right expectedEnv


prop_executeDoublePipeline :: Property
prop_executeDoublePipeline =
  shouldParseAndExecuteAs input expectedEnv
  where
    input =
      unlines
        [ "source input {"
        , "  value: 10"
        , "}"
        , "transform double from input"
        , "sink output from double"
        ]

    expectedEnv =
      Map.fromList
        [ ("input", NumVal 10)
        , ("double", NumVal 20)
        , ("output", NumVal 20)
        ]

prop_executeIncrementPipeline :: Property
prop_executeIncrementPipeline =
  shouldParseAndExecuteAs input expectedEnv
  where
    input =
      unlines
        [ "source input {"
        , "  value: 41"
        , "}"
        , "transform increment from input"
        , "sink output from increment"
        ]

    expectedEnv =
      Map.fromList
        [ ("input", NumVal 41)
        , ("increment", NumVal 42)
        , ("output", NumVal 42)
        ]

prop_executeUppercasePipeline :: Property
prop_executeUppercasePipeline =
  shouldParseAndExecuteAs input expectedEnv
  where
    input =
      unlines
        [ "source message {"
        , "  value: \"hello\""
        , "}"
        , "transform uppercase from message"
        , "sink output from uppercase"
        ]

    expectedEnv =
      Map.fromList
        [ ("message", StrVal "hello")
        , ("uppercase", StrVal "HELLO")
        , ("output", StrVal "HELLO")
        ]

prop_executeFilterPositivePipeline :: Property
prop_executeFilterPositivePipeline =
  shouldParseAndExecuteAs input expectedEnv
  where
    input =
      unlines
        [ "source numbers {"
        , "  value: [-2, 5, 0, 10, -1]"
        , "}"
        , "transform filterPositive from numbers"
        , "sink output from filterPositive"
        ]

    expectedEnv =
      Map.fromList
        [ ("numbers", ListVal [NumVal (-2), NumVal 5, NumVal 0, NumVal 10, NumVal (-1)])
        , ("filterPositive", ListVal [NumVal 5, NumVal 10])
        , ("output", ListVal [NumVal 5, NumVal 10])
        ]

prop_executeSumListPipeline :: Property
prop_executeSumListPipeline =
  shouldParseAndExecuteAs input expectedEnv
  where
    input =
      unlines
        [ "source numbers {"
        , "  value: [1, 2, 3, 4]"
        , "}"
        , "transform sum from numbers"
        , "sink output from sum"
        ]

    expectedEnv =
      Map.fromList
        [ ("numbers", ListVal [NumVal 1, NumVal 2, NumVal 3, NumVal 4])
        , ("sum", NumVal 10)
        , ("output", NumVal 10)
        ]

prop_executeFilterThenSumPipeline :: Property
prop_executeFilterThenSumPipeline =
  shouldParseAndExecuteAs input expectedEnv
  where
    input =
      unlines
        [ "source numbers {"
        , "  value: [-3, 1, 2, -5, 10]"
        , "}"
        , "transform filterPositive from numbers"
        , "transform sum from filterPositive"
        , "sink output from sum"
        ]

    expectedEnv =
      Map.fromList
        [ ("numbers", ListVal [NumVal (-3), NumVal 1, NumVal 2, NumVal (-5), NumVal 10])
        , ("filterPositive", ListVal [NumVal 1, NumVal 2, NumVal 10])
        , ("sum", NumVal 13)
        , ("output", NumVal 13)
        ]


prop_executeManualMultiInputSum :: Property
prop_executeManualMultiInputSum =
  shouldExecuteAs program expectedEnv
  where
    program =
      Program
        [ Node "a" Source [("value", NumVal 10)]
        , Node "b" Source [("value", NumVal 20)]
        , Node "sum" Transform []
        , Node "output" Sink []
        ]
        [ Edge "a" "sum"
        , Edge "b" "sum"
        , Edge "sum" "output"
        ]

    expectedEnv =
      Map.fromList
        [ ("a", NumVal 10)
        , ("b", NumVal 20)
        , ("sum", NumVal 30)
        , ("output", NumVal 30)
        ]

prop_rejectExecutionOfInvalidProgram :: Property
prop_rejectExecutionOfInvalidProgram =
  shouldFailWith program (ValidationFailed CycleDetected)
  where
    program =
      Program
        [ Node "a" Transform []
        , Node "b" Transform []
        ]
        [ Edge "a" "b"
        , Edge "b" "a"
        ]

prop_failUnknownPrimitive :: Property
prop_failUnknownPrimitive =
  shouldFailWith program (RuntimeFailed (UnknownPrimitive "unknown"))
  where
    program =
      Program
        [ Node "input" Source [("value", NumVal 10)]
        , Node "unknown" Transform []
        , Node "output" Sink []
        ]
        [ Edge "input" "unknown"
        , Edge "unknown" "output"
        ]

prop_failInvalidInputType :: Property
prop_failInvalidInputType =
  shouldFailWith program (RuntimeFailed (InvalidInputType "double expects a number"))
  where
    program =
      Program
        [ Node "text" Source [("value", StrVal "hello")]
        , Node "double" Transform []
        , Node "output" Sink []
        ]
        [ Edge "text" "double"
        , Edge "double" "output"
        ]

prop_failMissingSourceParameter :: Property
prop_failMissingSourceParameter =
  shouldFailWith program (RuntimeFailed (MissingParameter "input.value"))
  where
    program =
      Program
        [ Node "input" Source []
        , Node "output" Sink []
        ]
        [ Edge "input" "output" ]


runExecutorTests :: IO ()
runExecutorTests = do
  putStrLn "Executor tests"

  quickCheck prop_executeDoublePipeline
  quickCheck prop_executeIncrementPipeline
  quickCheck prop_executeUppercasePipeline
  quickCheck prop_executeFilterPositivePipeline
  quickCheck prop_executeSumListPipeline
  quickCheck prop_executeFilterThenSumPipeline
  quickCheck prop_executeManualMultiInputSum

  putStrLn "Executor error tests"

  quickCheck prop_rejectExecutionOfInvalidProgram
  quickCheck prop_failUnknownPrimitive
  quickCheck prop_failInvalidInputType
  quickCheck prop_failMissingSourceParameter