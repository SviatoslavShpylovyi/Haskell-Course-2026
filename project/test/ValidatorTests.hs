module ValidatorTests
  ( runValidatorTests
  ) where

import AST
import Errors
import Validator
import Test.QuickCheck


shouldValidate :: Program -> Property
shouldValidate program =
  validateProgram program === Right program

shouldFailWith :: Program -> ValidationError -> Property
shouldFailWith program expectedError =
  validateProgram program === Left expectedError

prop_acceptValidProgram :: Property
prop_acceptValidProgram =
  shouldValidate program
  where
    program =
      Program
        [ Node "input" Source [("value", NumVal 10)]
        , Node "double" Transform []
        , Node "output" Sink []
        ]
        [ Edge "input" "double"
        , Edge "double" "output"
        ]

prop_rejectDuplicateNodeIds :: Property
prop_rejectDuplicateNodeIds =
  shouldFailWith program (DuplicateNodeId "input")
  where
    program =
      Program
        [ Node "input" Source [("value", NumVal 10)]
        , Node "input" Transform []
        ]
        []

prop_rejectDanglingReferenceFrom :: Property
prop_rejectDanglingReferenceFrom =
  shouldFailWith program (DanglingReference "missing")
  where
    program =
      Program
        [ Node "output" Sink []
        ]
        [ Edge "missing" "output" ]

prop_rejectDanglingReferenceTo :: Property
prop_rejectDanglingReferenceTo =
  shouldFailWith program (DanglingReference "missing")
  where
    program =
      Program
        [ Node "input" Source [("value", NumVal 10)]
        ]
        [ Edge "input" "missing" ]

prop_rejectSourceWithInput :: Property
prop_rejectSourceWithInput =
  shouldFailWith program (InvalidSourceInput "input")
  where
    program =
      Program
        [ Node "previous" Transform []
        , Node "input" Source [("value", NumVal 10)]
        ]
        [ Edge "previous" "input" ]

prop_rejectSinkWithOutput :: Property
prop_rejectSinkWithOutput =
  shouldFailWith program (InvalidSinkOutput "output")
  where
    program =
      Program
        [ Node "output" Sink []
        , Node "next" Transform []
        ]
        [ Edge "output" "next" ]

prop_rejectCycle :: Property
prop_rejectCycle =
  shouldFailWith program CycleDetected
  where
    program =
      Program
        [ Node "a" Transform []
        , Node "b" Transform []
        ]
        [ Edge "a" "b"
        , Edge "b" "a"
        ]

prop_validateUniqueNodeIdsAcceptsUniqueIds :: Property
prop_validateUniqueNodeIdsAcceptsUniqueIds =
  validateUniqueNodeIds program === Right ()
  where
    program =
      Program
        [ Node "a" Source [("value", NumVal 1)]
        , Node "b" Transform []
        , Node "c" Sink []
        ]
        [ Edge "a" "b"
        , Edge "b" "c"
        ]

prop_validateEdgesAcceptsExistingReferences :: Property
prop_validateEdgesAcceptsExistingReferences =
  validateEdgesReferenceExistingNodes program === Right ()
  where
    program =
      Program
        [ Node "a" Source [("value", NumVal 1)]
        , Node "b" Sink []
        ]
        [ Edge "a" "b" ]

prop_validateNoCyclesAcceptsAcyclicGraph :: Property
prop_validateNoCyclesAcceptsAcyclicGraph =
  validateNoCycles program === Right ()
  where
    program =
      Program
        [ Node "a" Source [("value", NumVal 1)]
        , Node "b" Transform []
        , Node "c" Sink []
        ]
        [ Edge "a" "b"
        , Edge "b" "c"
        ]

prop_validateGeneratedLinearGraph :: Positive Int -> Property
prop_validateGeneratedLinearGraph (Positive rawSize) =
  validateProgram program === Right program
  where
    size =
      rawSize `mod` 20 + 2

    nodeName i =
      "n" ++ show i

    nodes =
      [ Node (nodeName 0) Source [("value", NumVal 1)]
      ]
      ++
      [ Node (nodeName i) Transform []
      | i <- [1 .. size - 2]
      ]
      ++
      [ Node (nodeName (size - 1)) Sink []
      ]

    edges =
      [ Edge (nodeName i) (nodeName (i + 1))
      | i <- [0 .. size - 2]
      ]

    program =
      Program nodes edges


prop_rejectGeneratedCycle :: Positive Int -> Property
prop_rejectGeneratedCycle (Positive rawSize) =
  validateProgram program === Left CycleDetected
  where
    size =
      rawSize `mod` 20 + 2

    nodeName i =
      "n" ++ show i

    nodes =
      [ Node (nodeName i) Transform []
      | i <- [0 .. size - 1]
      ]

    chainEdges =
      [ Edge (nodeName i) (nodeName (i + 1))
      | i <- [0 .. size - 2]
      ]

    cycleEdge =
      Edge (nodeName (size - 1)) (nodeName 0)

    program =
      Program nodes (chainEdges ++ [cycleEdge])

runValidatorTests :: IO ()
runValidatorTests = do
  putStrLn "Validator tests"

  quickCheck prop_acceptValidProgram
  quickCheck prop_rejectDuplicateNodeIds
  quickCheck prop_rejectDanglingReferenceFrom
  quickCheck prop_rejectDanglingReferenceTo
  quickCheck prop_rejectSourceWithInput
  quickCheck prop_rejectSinkWithOutput
  quickCheck prop_rejectCycle

  putStrLn "Individual validator function tests"

  quickCheck prop_validateUniqueNodeIdsAcceptsUniqueIds
  quickCheck prop_validateEdgesAcceptsExistingReferences
  quickCheck prop_validateNoCyclesAcceptsAcyclicGraph
  putStrLn "Generated Graphs"
  quickCheck prop_validateGeneratedLinearGraph
  quickCheck prop_rejectGeneratedCycle