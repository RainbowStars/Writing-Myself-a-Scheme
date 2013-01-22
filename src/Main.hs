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
        <|> parseSharp
        <|> parseNumber
        <|> parseString
        <|> parseQuoted
        <|> do
            char '('
            val <- (try parseList) <|> parseDottedList
            char ')'
            return val

parseSharp :: Parser LispVal
parseSharp = do
    char '#'
    x <- anyChar
    xs <- many (noneOf " ")
    return (v x xs) where
        v x xs | x == 't'      = Bool True
               | x == 'f'      = Bool False
               | x =='\\'      = Character (parseCharacter x xs)
               | elem x "bodx" = Number (parseSharpNumber x xs)
               | otherwise     = error "Invalid sharp expression."
            where
                parseCharacter :: Char -> [Char] -> Char
                parseCharacter _ name | (length name) == 1 = head name
                                      | name == "space" = ' '
                                      | name == "newline" = '\n'
                                      | otherwise = error "Invalid character expression."
                parseSharpNumber :: Char -> [Char] -> Integer
                parseSharpNumber base num | num == [] = 0
                                          | otherwise = let
                    num' = case base of
                        'b' -> readBin num
                        'o' -> readOct num
                        'd' -> readDec num
                        'x' -> readHex num
                    in (v num') where
                        v n | snd (head n) /= [] = error "Invalid numeric expression."
                        v n | n  == [] = error "Invalid numeric expression."
                        v n | otherwise = fst (head n)
        

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return (Atom atom)

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return (DottedList head tail)

parseList :: Parser LispVal
parseList = liftM List (sepBy parseExpr spaces)

parseNumber :: Parser LispVal
parseNumber = do
    number <- many1 digit
    return (Number (read number))

--parseNumber = (many1 digit) >>= liftM (Number . read)
--parseNumber = do
--    number <- many1 digit
--    return (Number (read number))
--parseNumber = liftM (Number . read) (many1 digit)

parseQuoted :: Parser LispVal
parseQuoted = do
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