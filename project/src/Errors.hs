module Errors where

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