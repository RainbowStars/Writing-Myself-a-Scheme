{-# OPTIONS -XFlexibleInstances #-}
module Main where

import Control.Monad
import Data.Array
import Data.Array.IO
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | Bool Bool
             | Character Char
             | DottedList [LispVal] LispVal
             | List [LispVal]
             | Number Integer
             | String String
             -- | Vector (IOArray Int LispVal)
             | Vector [LispVal]

data LispNum = Complex Float Float
             | Real Float
             | Rational Integer Integer
             | Integer Integer
               deriving (Show)

instance Show LispVal where
--    show :: LispVal -> String
    show (Atom v) = v
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Character char) = "#\\" ++ [char]
    show (DottedList head tail) = ("(" ++ (unwords . map show) head ++ " . " ++ show tail ++ ")")
    show (List list) = "(" ++ (unwords . map show) list ++ ")"
    show (Number n) = show n
    show (String s) = "\"" ++ s ++ "\""
    show (Vector vector) = "#(" ++ (unwords . map show) vector ++ ")"
    show n = error "Undefined instance of Show LispVal"

instance Show (IOArray Int LispVal) where
    show _ = "oh dear"

main :: IO ()
main = do
    args <- getArgs
    val <- (return . show . eval . readExpr) (args !! 0)
    putStrLn val

eval :: LispVal -> LispVal
eval val@(Bool _) = val
eval val@(Number _) = val
eval val@(String _) = val
eval (List [Atom "quote", val]) = val

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left e  -> String ("No match: "    ++ show e)
    Right v -> v

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseSharp
        <|> parseNumber
        <|> parseString
        <|> parseQuote
        <|> between (char '(') (char ')') parseList

parseSharp :: Parser LispVal
parseSharp = do
    char '#'
    val <- (parseBool <|> parseCharacter <|> parseSharpNumber <|> parseVector)
    return val where
        parseBool :: Parser LispVal
        parseBool = (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))
        
        parseCharacter :: Parser LispVal
        parseCharacter = do
            char '\\'
            name <- many (noneOf " )")
            (return . Character) (parseCharacter' name) where
                parseCharacter' [c] = c
                parseCharacter' cs  = case cs of
                    "space"   -> ' '
                    "newline" -> '\n'
                    otherwise -> error "Invalid character expression."
        
        parseSharpNumber :: Parser LispVal
        parseSharpNumber = do
            base <- oneOf "bodx"
            num  <- many (noneOf " ")
            (return . Number) (parseSharpNumber' base num) where
                parseSharpNumber' base []  = error "Invalid numeric expression."
                parseSharpNumber' base num = case (readNum base num) of
                    [(n,[])]   -> n
                    _          -> error "Invalid numeric expression."
                    where
                        readNum 'b' num = readBin num
                        readNum 'o' num = readOct num
                        readNum 'd' num = readDec num
                        readNum 'x' num = readHex num
                        readNum _   num = error "Invalid numeric expression."
        
        parseVector :: Parser LispVal
        parseVector = do
            list <- between (char '(') (char ')') parseList
            let l (List xs) = xs
                l _         = error "Invalid vector expression"
            (return . Vector) (l list)
        {-
        parseVector = do
            list <- between (char '(') (char ')') parseList
            array <- case list of
                (List l) -> newListArray (1, length l) l -- :: Parser (IOArray Int LispVal)
                _        -> error "Invalid vector expression"
            return (Number 0)
        -}

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

parseQuote :: Parser LispVal
parseQuote = do
    c <- oneOf "\'`,"
    expr <- parseExpr
    (return . List) $ case c of
        '\'' -> [Atom "quote", expr]
        ','  -> [Atom "unquote", expr]
        '`'  -> [Atom "quasiquote", expr]

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