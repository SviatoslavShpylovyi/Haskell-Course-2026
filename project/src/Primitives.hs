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
  case nodeId node of
    "camera" ->
      runCameraSource node

    _ ->
      case lookup "value" (nodeParams node) of
        Just value ->
          Right value

        Nothing ->
          Left (MissingParameter (nodeId node ++ ".value"))


runCameraSource :: Node -> Either RuntimeError Value
runCameraSource node = do
  width <- requireNumberParam node "width"
  height <- requireNumberParam node "height"
  format <- requireStringParam node "format"

  Right
    ( StrVal
        ( "Image(width="
            ++ show width
            ++ ", height="
            ++ show height
            ++ ", format="
            ++ format
            ++ ")"
        )
    )


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
    
    "resize" ->
      do
        input <- requireSingleInput "resize" inputs
        resizeValue node input

    "grayscale" ->
      do
        input <- requireSingleInput "grayscale" inputs
        grayscaleValue input

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

resizeValue :: Node -> Value -> Either RuntimeError Value
resizeValue node value =
  case value of
    StrVal imageDescription -> do
      width <- requireNumberParam node "width"
      height <- requireNumberParam node "height"

      Right
        ( StrVal
            ( imageDescription
                ++ " -> Resized(width="
                ++ show width
                ++ ", height="
                ++ show height
                ++ ")"
            )
        )

    _ ->
      Left (InvalidInputType "resize expects an image value")


grayscaleValue :: Value -> Either RuntimeError Value
grayscaleValue value =
  case value of
    StrVal imageDescription ->
      Right (StrVal (imageDescription ++ " -> Grayscale"))

    _ ->
      Left (InvalidInputType "grayscale expects an image value")


requireNumberParam :: Node -> String -> Either RuntimeError Double
requireNumberParam node paramName =
  case lookup paramName (nodeParams node) of
    Just (NumVal number) ->
      Right number

    Just _ ->
      Left
        ( InvalidInputType
            (nodeId node ++ "." ++ paramName ++ " must be a number")
        )

    Nothing ->
      Left (MissingParameter (nodeId node ++ "." ++ paramName))


requireStringParam :: Node -> String -> Either RuntimeError String
requireStringParam node paramName =
  case lookup paramName (nodeParams node) of
    Just (StrVal text) ->
      Right text

    Just _ ->
      Left
        ( InvalidInputType
            (nodeId node ++ "." ++ paramName ++ " must be a string")
        )

    Nothing ->
      Left (MissingParameter (nodeId node ++ "." ++ paramName))