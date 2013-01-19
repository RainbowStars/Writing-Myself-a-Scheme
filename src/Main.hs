module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left e  -> "No match: " ++ show e
    Right x -> "Found value"

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"