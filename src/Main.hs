module Main where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left e  -> "No match: " ++ show e
    Right x -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseNumber
        <|> parseString

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return (case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        otherwise -> Atom atom)

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)

parseString :: Parser LispVal
parseString = do
    char '"'
    str <- many (noneOf "\"")
    char '"'
    return (String str)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser()
spaces = skipMany1 space