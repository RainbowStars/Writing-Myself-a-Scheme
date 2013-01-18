module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Hello, " ++ args !! 0 ++ " " ++ args !! 1)