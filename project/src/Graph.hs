module Graph
  ( NodeId
  , nodeIds
  , nodeMap
  , adjacencyMap
  , incomingMap
  , outgoingEdges
  , incomingEdges
  , dependenciesOf
  , dependentsOf
  , topologicalSort
  ) where

import AST
import Errors

import Data.List (foldl')
import qualified Data.Map.Strict as Map

type NodeId = String

nodeIds :: Program -> [NodeId]
nodeIds (Program nodes _) =
  map nodeId nodes

nodeMap :: Program -> Map.Map NodeId Node
nodeMap (Program nodes _) =
  Map.fromList [(nodeId node, node) | node <- nodes]

adjacencyMap :: Program -> Map.Map NodeId [NodeId]
adjacencyMap program@(Program _ edges) =
  foldl' addEdge initialMap edges
  where
    initialMap =
      Map.fromList [(nodeIdValue, []) | nodeIdValue <- nodeIds program]

    addEdge currentMap edge =
      Map.insertWith
        (++)
        (edgeFrom edge)
        [edgeTo edge]
        currentMap

incomingMap :: Program -> Map.Map NodeId [NodeId]
incomingMap program@(Program _ edges) =
  foldl' addEdge initialMap edges
  where
    initialMap =
      Map.fromList [(nodeIdValue, []) | nodeIdValue <- nodeIds program]

    addEdge currentMap edge =
      Map.insertWith
        (++)
        (edgeTo edge)
        [edgeFrom edge]
        currentMap

outgoingEdges :: Program -> NodeId -> [Edge]
outgoingEdges (Program _ edges) searchedNodeId =
  filter (\edge -> edgeFrom edge == searchedNodeId) edges

incomingEdges :: Program -> NodeId -> [Edge]
incomingEdges (Program _ edges) searchedNodeId =
  filter (\edge -> edgeTo edge == searchedNodeId) edges

dependenciesOf :: Program -> NodeId -> [NodeId]
dependenciesOf program searchedNodeId =
  Map.findWithDefault [] searchedNodeId (incomingMap program)

dependentsOf :: Program -> NodeId -> [NodeId]
dependentsOf program searchedNodeId =
  Map.findWithDefault [] searchedNodeId (adjacencyMap program)

topologicalSort :: Program -> Either ValidationError [Node]
topologicalSort program@(Program nodes edges) = do
  sortedNodeIds <- topologicalSortIds program
  mapNodeIdsToNodes sortedNodeIds
  where
    nodesById =
      Map.fromList [(nodeId node, node) | node <- nodes]

    mapNodeIdsToNodes :: [NodeId] -> Either ValidationError [Node]
    mapNodeIdsToNodes [] =
      Right []

    mapNodeIdsToNodes (currentId : rest) =
      case Map.lookup currentId nodesById of
        Nothing ->
          Left (DanglingReference currentId)

        Just currentNode -> do
          remainingNodes <- mapNodeIdsToNodes rest
          Right (currentNode : remainingNodes)

topologicalSortIds :: Program -> Either ValidationError [NodeId]
topologicalSortIds program@(Program nodes edges) =
  process initialInDegree initialReadyNodes []
  where
    allNodeIds =
      map nodeId nodes

    initialInDegree =
      foldl' increaseInDegree emptyInDegree edges

    emptyInDegree =
      Map.fromList [(currentNodeId, 0 :: Int) | currentNodeId <- allNodeIds]

    increaseInDegree currentMap edge =
      Map.adjust (+ 1) (edgeTo edge) currentMap

    initialReadyNodes =
      [ currentNodeId
      | (currentNodeId, degree) <- Map.toList initialInDegree
      , degree == 0
      ]

    process :: Map.Map NodeId Int -> [NodeId] -> [NodeId] -> Either ValidationError [NodeId]
    process _ [] sortedIds
      | length sortedIds == length nodes =
          Right (reverse sortedIds)

      | otherwise =
          Left CycleDetected

    process currentInDegree (currentNodeId : queue) sortedIds =
      let nextNodes =
            dependentsOf program currentNodeId

          (updatedInDegree, newlyReadyNodes) =
            foldl'
              reduceInDegree
              (currentInDegree, [])
              nextNodes

          updatedQueue =
            queue ++ newlyReadyNodes

          updatedSortedIds =
            currentNodeId : sortedIds
      in
        process updatedInDegree updatedQueue updatedSortedIds

    reduceInDegree ::
      (Map.Map NodeId Int, [NodeId])
      -> NodeId
      -> (Map.Map NodeId Int, [NodeId])
    reduceInDegree (currentMap, readyNodes) dependentNodeId =
      let oldDegree =
            Map.findWithDefault 0 dependentNodeId currentMap

          newDegree =
            oldDegree - 1

          updatedMap =
            Map.insert dependentNodeId newDegree currentMap
      in
        if newDegree == 0
          then (updatedMap, readyNodes ++ [dependentNodeId])
          else (updatedMap, readyNodes)