module Executor
  ( Env
  , executeProgram
  ) where

import AST
import Errors
import Graph
import Primitives
import Validator

import qualified Data.Map.Strict as Map

type Env = Map.Map String Value

executeProgram :: Program -> Either ExecutionError Env
executeProgram program = do
  validProgram <- mapValidationError (validateProgram program)
  sortedNodes <- mapValidationError (topologicalSort validProgram)
  mapRuntimeError (executeNodes validProgram sortedNodes Map.empty)

executeNodes :: Program -> [Node] -> Env -> Either RuntimeError Env
executeNodes _ [] env =
  Right env

executeNodes program (currentNode : rest) env = do
  updatedEnv <- executeNode program env currentNode
  executeNodes program rest updatedEnv

executeNode :: Program -> Env -> Node -> Either RuntimeError Env
executeNode program env node =
  case nodeKind node of
    Source ->
      do
        output <- runSource node
        Right (Map.insert (nodeId node) output env)

    Transform ->
      do
        inputs <- getInputValues program env node
        output <- runTransform node inputs
        Right (Map.insert (nodeId node) output env)

    Sink ->
      do
        inputs <- getInputValues program env node
        output <- runSink node inputs
        Right (Map.insert (nodeId node) output env)

getInputValues :: Program -> Env -> Node -> Either RuntimeError [Value]
getInputValues program env node =
  collectValues dependencyIds
  where
    dependencyIds =
      dependenciesOf program (nodeId node)

    collectValues :: [String] -> Either RuntimeError [Value]
    collectValues [] =
      Right []

    collectValues (dependencyId : rest) =
      case Map.lookup dependencyId env of
        Nothing ->
          Left (MissingInput dependencyId)

        Just value ->
          do
            remainingValues <- collectValues rest
            Right (value : remainingValues)

mapValidationError :: Either ValidationError a -> Either ExecutionError a
mapValidationError result =
  case result of
    Left validationError ->
      Left (ValidationFailed validationError)

    Right value ->
      Right value

mapRuntimeError :: Either RuntimeError a -> Either ExecutionError a
mapRuntimeError result =
  case result of
    Left runtimeError ->
      Left (RuntimeFailed runtimeError)

    Right value ->
      Right value