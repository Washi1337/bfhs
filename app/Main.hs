module Main where

import BfHs.Language
import BfHs.Tape
import BfHs.Interpreter

import Data.Either 
import Data.Char


defaultIO :: IODevice IO Int
defaultIO = IODevice (writeToStdOutput) (readFromStdInput)
    where 
        writeToStdOutput x = putChar $ chr x
        readFromStdInput  = do 
            str <- getLine
            return $ ord $ head str

main :: IO ()
main = do
    putStrLn "Code: "
    code <- getLine
    resultTape <- eval defaultIO tape (fromRight [] $ parseBrainFuck code)
    putStrLn $ show resultTape
    where tape = boundedTape 100 :: Tape Int

