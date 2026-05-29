module Primitives
  ( runSource
  , runTransform
  , runSink
  ) where

import AST
import Errors

import Data.Char (toUpper)

runSource :: Node ->Either RuntimeError Value
runSource node = 
    case lookup "value" (nodeParams node) of 
        Just value ->
            Right value
        Nothing ->
            Left (MissingParameter (nodeId node ++ ".value"))

runTransform :: Node -> [Value] -> Either RuntimeError Value
runTransform node inputs = 
    case nodeId node of
    "double" ->
      do
        input <- requireSingleInput "double" inputs
        doubleValue input

    "increment" ->
      do
        input <- requireSingleInput "increment" inputs
        incrementValue input

    "uppercase" ->
      do
        input <- requireSingleInput "uppercase" inputs
        uppercaseValue input

    "filterPositive" ->
      do
        input <- requireSingleInput "filterPositive" inputs
        filterPositiveValue input

    "sum" ->
      sumValues inputs

    unknownName ->
      Left (UnknownPrimitive unknownName)

runSink :: Node -> [Value] -> Either RuntimeError Value
runSink node inputs =
  requireSingleInput (nodeId node) inputs

requireSingleInput :: String -> [Value] -> Either RuntimeError Value
requireSingleInput primitiveName inputs =
  case inputs of
    [value] ->
      Right value

    [] ->
      Left (MissingInput primitiveName)

    _ ->
      Left (InvalidInputType (primitiveName ++ " expects exactly one input"))

doubleValue :: Value -> Either RuntimeError Value
doubleValue value =
  case value of
    NumVal number ->
      Right (NumVal (number * 2))

    _ ->
      Left (InvalidInputType "double expects a number")

incrementValue :: Value -> Either RuntimeError Value
incrementValue value =
  case value of
    NumVal number ->
      Right (NumVal (number + 1))

    _ ->
      Left (InvalidInputType "increment expects a number")

uppercaseValue :: Value -> Either RuntimeError Value
uppercaseValue value =
  case value of
    StrVal text ->
      Right (StrVal (map toUpper text))

    _ ->
      Left (InvalidInputType "uppercase expects a string")

filterPositiveValue :: Value -> Either RuntimeError Value
filterPositiveValue value =
  case value of
    ListVal values ->
      ListVal <$> filterPositiveList values

    _ ->
      Left (InvalidInputType "filterPositive expects a list of numbers")

filterPositiveList :: [Value] -> Either RuntimeError [Value]
filterPositiveList [] =
  Right []

filterPositiveList (value : rest) =
  case value of
    NumVal number ->
      do
        filteredRest <- filterPositiveList rest
        if number > 0
          then Right (NumVal number : filteredRest)
          else Right filteredRest

    _ ->
      Left (InvalidInputType "filterPositive expects only numbers inside the list")

sumValues :: [Value] -> Either RuntimeError Value
sumValues inputs =
  case inputs of
    [ListVal values] ->
      NumVal <$> sumNumberList values

    values ->
      NumVal <$> sumNumberList values

sumNumberList :: [Value] -> Either RuntimeError Double
sumNumberList [] =
  Right 0

sumNumberList (value : rest) =
  case value of
    NumVal number ->
      do
        restSum <- sumNumberList rest
        Right (number + restSum)

    _ ->
      Left (InvalidInputType "sum expects numbers")
