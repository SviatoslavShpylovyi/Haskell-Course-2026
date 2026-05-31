module Main where

import ParserTests
import ValidatorTests
import ExecutorTests

main :: IO ()
main = do
   runParserTests
   runValidatorTests
   runExecutorTests