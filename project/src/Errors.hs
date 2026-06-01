module Errors where

data ParseError = ParseError
  { errorLine     :: Int
  , errorColumn   :: Int
  , errorExpected :: String
  , errorFound    :: String
  }
  deriving (Show, Eq)

prettyParseError :: ParseError -> String
prettyParseError err =
  "Syntax error at line "
    ++ show (errorLine err)
    ++ ", column "
    ++ show (errorColumn err)
    ++ ". Expected "
    ++ errorExpected err
    ++ ", but found "
    ++ errorFound err
    ++ "."
    
data ValidationError
  = DuplicateNodeId String
  | DanglingReference String
  | CycleDetected
  | InvalidSourceInput String
  | InvalidSinkOutput String
  deriving (Show, Eq)

data RuntimeError
  = MissingInput String
  | UnknownPrimitive String
  | InvalidInputType String
  | MissingParameter String
  deriving (Show, Eq)

data ExecutionError
  = ValidationFailed ValidationError
  | RuntimeFailed RuntimeError
  deriving (Show, Eq)