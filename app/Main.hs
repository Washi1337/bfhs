module Main where

import BfHs.Language
import BfHs.Tape
import BfHs.Interpreter

import Data.Either 

main :: IO ()
main = do
    putStrLn "Code: "
    code <- getLine
    resultTape <- evalProgram tape (fromRight [] $ parseBrainFuck code)
    putStrLn $ show resultTape
    where tape = boundedTape 100 :: Tape Int
