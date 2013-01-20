module Main where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

type Real = Float

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
               deriving (Show)

data LispNum = Complex Float Float
             | Real Float
             | Rational Float Float
             | Integer Integer
               deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left e  -> "No match: " ++ show e
    Right x -> "Found value: " ++ (show x)

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
--parseNumber = (many1 digit) >>= liftM (Number . read)
--parseNumber = do
--    number <- many1 digit
--    return (Number (read number))
parseNumber = liftM (Number . read) (many1 digit)

parseString :: Parser LispVal
parseString = do
    char '"'
    str <- many $ do
        char '\\'
        char' <- anyChar
        return (case char' of
            '\\' -> '\\'
            '\"' -> '\"'
            'n'  -> '\n'
            'r'  -> '\r'
            't'  -> '\t')
        <|> noneOf "\""
    char '"'
    return (String str)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser()
spaces = skipMany1 space