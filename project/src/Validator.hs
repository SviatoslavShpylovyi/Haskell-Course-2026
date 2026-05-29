module Validator
  ( validateProgram
  , validateUniqueNodeIds
  , validateEdgesReferenceExistingNodes
  , validateSourceNodesHaveNoInputs
  , validateSinkNodesHaveNoOutputs
  , validateNoCycles
  ) where

import AST
import Errors
import Graph

import qualified Data.Set as Set

validateProgram :: Program -> Either ValidationError Program
validateProgram program = do
  validateUniqueNodeIds program
  validateEdgesReferenceExistingNodes program
  validateSourceNodesHaveNoInputs program
  validateSinkNodesHaveNoOutputs program
  validateNoCycles program
  Right program

validateUniqueNodeIds :: Program -> Either ValidationError ()
validateUniqueNodeIds (Program nodes _) =
  checkNodes Set.empty nodes
  where
    checkNodes :: Set.Set String -> [Node] -> Either ValidationError ()
    checkNodes _ [] =
      Right ()

    checkNodes seenIds (currentNode : rest) =
      let currentId =
            nodeId currentNode
      in
        if Set.member currentId seenIds
          then Left (DuplicateNodeId currentId)
          else checkNodes (Set.insert currentId seenIds) rest

validateEdgesReferenceExistingNodes :: Program -> Either ValidationError ()
validateEdgesReferenceExistingNodes program@(Program _ edges) =
  checkEdges edges
  where
    existingNodeIds =
      Set.fromList (nodeIds program)

    checkEdges :: [Edge] -> Either ValidationError ()
    checkEdges [] =
      Right ()

    checkEdges (currentEdge : rest)
      | not (Set.member (edgeFrom currentEdge) existingNodeIds) =
          Left (DanglingReference (edgeFrom currentEdge))

      | not (Set.member (edgeTo currentEdge) existingNodeIds) =
          Left (DanglingReference (edgeTo currentEdge))

      | otherwise =
          checkEdges rest

validateSourceNodesHaveNoInputs :: Program -> Either ValidationError ()
validateSourceNodesHaveNoInputs program@(Program nodes _) =
  checkNodes nodes
  where
    checkNodes :: [Node] -> Either ValidationError ()
    checkNodes [] =
      Right ()

    checkNodes (currentNode : rest) =
      case nodeKind currentNode of
        Source ->
          if null (incomingEdges program (nodeId currentNode))
            then checkNodes rest
            else Left (InvalidSourceInput (nodeId currentNode))

        _ ->
          checkNodes rest

validateSinkNodesHaveNoOutputs :: Program -> Either ValidationError ()
validateSinkNodesHaveNoOutputs program@(Program nodes _) =
  checkNodes nodes
  where
    checkNodes :: [Node] -> Either ValidationError ()
    checkNodes [] =
      Right ()

    checkNodes (currentNode : rest) =
      case nodeKind currentNode of
        Sink ->
          if null (outgoingEdges program (nodeId currentNode))
            then checkNodes rest
            else Left (InvalidSinkOutput (nodeId currentNode))

        _ ->
          checkNodes rest

validateNoCycles :: Program -> Either ValidationError ()
validateNoCycles program =
  case topologicalSort program of
    Left CycleDetected ->
      Left CycleDetected

    Left otherError ->
      Left otherError

    Right _ ->
      Right ()