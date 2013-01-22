module Main where

import Control.Monad
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

type Real = Float

data LispVal = Atom String
             | Bool Bool
             | Character Char
             | DottedList [LispVal] LispVal
             | List [LispVal]
             | Number Integer
             | String String
               deriving (Show)

data LispNum = Complex Float Float
             | Real Float
             | Rational Integer Integer
             | Integer Integer
               deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left e  -> "No match: "    ++ show e
    Right e -> "Found value: " ++ show e

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseSharp
        <|> parseNumber
        <|> parseString
--        <|> parseQuasiquote
        <|> parseQuote
        <|> between (char '(') (char ')') parseList

parseSharp :: Parser LispVal
parseSharp = do
    char '#'
    x <- anyChar
    xs <- many (noneOf " ")
    let val | x == 't'      = Bool True
            | x == 'f'      = Bool False
            | x =='\\'      = Character (parseCharacter x xs)
            | elem x "bodx" = Number (parseSharpNumber x xs)
            | otherwise     = error "Invalid sharp expression."
    return val where
        parseCharacter :: Char -> [Char] -> Char
        parseCharacter _ [c] = c
        parseCharacter _ cs  = case cs of
            "space"   -> ' '
            "newline" -> '\n'
            otherwise -> error "Invalid character expression."
        
        parseSharpNumber :: Char -> [Char] -> Integer
        parseSharpNumber base []  = error "Invalid numeric expression."
        parseSharpNumber base num = case (parseSharpNumber' base num) of
            [(n,[])]   -> n
            _          -> error "Invalid numeric expression."
            where
                parseSharpNumber' 'b' num = readBin num
                parseSharpNumber' 'o' num = readOct num
                parseSharpNumber' 'd' num = readDec num
                parseSharpNumber' 'x' num = readHex num
                parseSharpNumber' _   num = error "Invalid numeric expression."

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return (Atom (first:rest))

parseList :: Parser LispVal
parseList = do
    head <- sepEndBy1 parseExpr spaces
    let 
        parseDottedList = do
            tail <- char '.' >> spaces >> parseExpr
            return (DottedList head tail)
        parseList = do
            tail <- sepEndBy parseExpr spaces
            return (List (head ++ tail))
    parseDottedList <|> parseList

parseNumber :: Parser LispVal
parseNumber = do
    number <- many1 digit
    return (Number (read number))

{-
parseQuasiquote :: Parser LispVal
parseQuasiquote = parseQuasiquote' <|> parseUnquasiquote where
    parseQuasiquote = 
    
    parseUnquasiquote = 
-}

parseQuote :: Parser LispVal
parseQuote = do
    char '\''
    expr <- parseExpr
    return (List [Atom "quote", expr])

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
            't'  -> '\t'
            _    -> char')
        <|> noneOf "\""
    char '"'
    return (String str)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readBin :: [Char] -> [(Integer, [Char])]
readBin xs = readBin' xs 0 [] where
    readBin'    xs  y zs | xs == []  = [(y, zs)]
    readBin' (x:xs) y zs | x  == '1' = readBin' xs (2^(length xs) + y) zs
    readBin' (x:xs) y zs | x  == '0' = readBin' xs y zs
    readBin' (x:xs) y zs | otherwise = readBin' xs y (x:zs)