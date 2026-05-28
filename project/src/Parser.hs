module Parser (Parser, runParser, parseProgram, program Parser)where
import AST
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
