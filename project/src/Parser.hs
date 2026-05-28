module Parser
  ( Parser
  , runParser
  , parseProgram
  , programParser
  ) where

import AST

import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

type Parser a = StateT String [] a

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT

zero :: Parser a
zero = StateT (const [])

item :: Parser Char
item = do
  input <- get
  case input of
    [] -> zero
    c : rest -> do
      put rest
      pure c

infixr 5 <|>

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = StateT $ \input ->
  case runStateT p1 input of
    [] -> runStateT p2 input
    results -> results

sat :: (Char -> Bool) -> Parser Char
sat predicate = do
  c <- item
  if predicate c
    then pure c
    else zero

char :: Char -> Parser Char
char expected = sat (== expected)

string :: String -> Parser String
string [] = pure []
string (c : cs) = do
  _ <- char c
  _ <- string cs
  pure (c : cs)

digit :: Parser Char
digit = sat isDigit

spaceP :: Parser Char
spaceP = sat isSpace

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  pure (x : xs)

optionalP :: Parser a -> Parser (Maybe a)
optionalP p = (Just <$> p) <|> pure Nothing

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many $ do
    _ <- sep
    p
  pure (x : xs)

-- Whitespace and comments

lineComment :: Parser ()
lineComment = do
  _ <- string "//"
  _ <- many (sat (/= '\n'))
  _ <- optionalP (char '\n')
  pure ()

blockComment :: Parser ()
blockComment = do
  _ <- string "/*"
  go
  where
    go =
          (string "*/" >> pure ())
      <|> (item >> go)

spaceUnit :: Parser ()
spaceUnit =
      (spaceP >> pure ())
  <|> lineComment
  <|> blockComment

spaces :: Parser ()
spaces = do
  _ <- many spaceUnit
  pure ()

-- Tokens

token :: Parser a -> Parser a
token p = do
  value <- p
  spaces
  pure value

symbol :: String -> Parser String
symbol text = token (string text)

eof :: Parser ()
eof = do
  input <- get
  case input of
    [] -> pure ()
    _  -> zero

-- Keywords and identifiers

identStart :: Parser Char
identStart = sat (\c -> isAlpha c || c == '_')

identChar :: Parser Char
identChar = sat (\c -> isAlphaNum c || c == '_' || c == '-')

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '-'

keyword :: String -> Parser String
keyword word = token $ do
  parsed <- string word
  rest <- get
  case rest of
    c : _ | isIdentChar c -> zero
    _ -> pure parsed

identifier :: Parser String
identifier = token $ do
  first <- identStart
  rest <- many identChar
  let name = first : rest
  if name `elem` reservedWords
    then zero
    else pure name

reservedWords :: [String]
reservedWords =
  [ "source"
  , "transform"
  , "sink"
  , "from"
  , "true"
  , "false"
  ]

-- Values

parseStringValue :: Parser Value
parseStringValue = token $ do
  _ <- char '"'
  content <- many stringChar
  _ <- char '"'
  pure (StrVal content)

stringChar :: Parser Char
stringChar =
      escapedChar
  <|> sat (\c -> c /= '"' && c /= '\\')

escapedChar :: Parser Char
escapedChar = do
  _ <- char '\\'
  escaped <- item
  case escaped of
    'n'  -> pure '\n'
    't'  -> pure '\t'
    '"'  -> pure '"'
    '\\' -> pure '\\'
    other -> pure other

parseNumberValue :: Parser Value
parseNumberValue = token $ do
  signPart <- optionalP (char '-')
  wholePart <- many1 digit

  decimalPart <- optionalP $ do
    dot <- char '.'
    digits <- many1 digit
    pure (dot : digits)

  let numberText =
        maybe "" (: []) signPart
        ++ wholePart
        ++ maybe "" id decimalPart

  pure (NumVal (read numberText))

parseBoolValue :: Parser Value
parseBoolValue =
      (keyword "true" >> pure (BoolVal True))
  <|> (keyword "false" >> pure (BoolVal False))

parseListValue :: Parser Value
parseListValue = do
  _ <- symbol "["
  values <- parseValue `sepBy` symbol ","
  _ <- symbol "]"
  pure (ListVal values)

parseValue :: Parser Value
parseValue =
      parseStringValue
  <|> parseBoolValue
  <|> parseNumberValue
  <|> parseListValue

-- Parameters

parseParam :: Parser (String, Value)
parseParam = do
  key <- identifier
  _ <- symbol ":"
  value <- parseValue
  _ <- optionalP (symbol ",")
  pure (key, value)

parseParams :: Parser [(String, Value)]
parseParams =
      parseParamBlock
  <|> pure []

parseParamBlock :: Parser [(String, Value)]
parseParamBlock = do
  _ <- symbol "{"
  params <- many parseParam
  _ <- symbol "}"
  pure params

-- Nodes

parseNodeKind :: Parser NodeKind
parseNodeKind =
      (keyword "source" >> pure Source)
  <|> (keyword "transform" >> pure Transform)
  <|> (keyword "sink" >> pure Sink)

parseFrom :: Parser String
parseFrom = do
  _ <- keyword "from"
  identifier

parseDeclaration :: Parser (Node, [Edge])
parseDeclaration = do
  kind <- parseNodeKind
  name <- identifier
  inputNode <- optionalP parseFrom
  params <- parseParams

  let node =
        Node
          { nodeId = name
          , nodeKind = kind
          , nodeParams = params
          }

  let edges =
        case inputNode of
          Nothing -> []
          Just sourceName -> [Edge sourceName name]

  pure (node, edges)

-- Full program parser

programParser :: Parser Program
programParser = do
  spaces
  declarations <- many parseDeclaration
  eof

  let nodes = map fst declarations
  let edges = concatMap snd declarations

  pure (Program nodes edges)

parseProgram :: String -> Maybe Program
parseProgram input =
  case runParser programParser input of
    (program, "") : _ -> Just program
    _ -> Nothing