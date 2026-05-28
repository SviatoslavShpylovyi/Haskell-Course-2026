module AST where
data Program = Program [Node] [Edge]
    deriving (Show, Eq)
data Node = Node
  { nodeId     :: String
  , nodeKind   :: NodeKind
  , nodeParams :: [(String, Value)]
  } deriving (Show, Eq)
data NodeKind
  = Source
  | Transform
  | Sink
  deriving (Show, Eq)
data Edge = Edge
  { edgeFrom :: String
  , edgeTo   :: String
  } deriving (Show, Eq)

data Value
  = StrVal String
  | NumVal Double
  | BoolVal Bool
  | ListVal [Value]
  deriving (Show, Eq)