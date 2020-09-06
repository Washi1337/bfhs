module Main where

import BfHs.Language 
    ( parseBrainFuck,
      BfProgram )
    
import BfHs.Tape 
    ( Tape, 
      boundedTape, 
      contents )

import BfHs.Interpreter
    ( eval, 
      IODevice(IODevice) )

import BfHs.Transpiler
    ( transpile )

import Data.Either

import Data.Char 
    ( ord, 
      chr )

import System.Environment 
    ( getArgs ) 

import System.IO
    ( stdout,
      hClose,
      hFlush,
      openFile,
      hGetContents,
      IOMode(ReadMode) )

tapeSize :: Int
tapeSize = 100

defaultIO :: IODevice IO Int
defaultIO = IODevice (writeToStdOutput) (readFromStdInput)
    where 
        writeToStdOutput x = putChar $ chr x
        readFromStdInput  = do 
            c <- getChar
            return $ ord $ c

main :: IO ()
main = do
    args <- getArgs
    mainWithArgs args

mainWithArgs :: [String] -> IO ()
mainWithArgs []                     = mainInteractive
mainWithArgs ["--help"]             = mainHelp
mainWithArgs ["--interactive"]      = mainInteractive
mainWithArgs ["--transpile", file]  = mainTranspile file
mainWithArgs ["--eval", file]       = mainEval file
mainWithArgs [file]                 = mainEval file
mainWithArgs _                      = mainHelp

mainInteractive :: IO ()
mainInteractive = do 
    putStr "(fuck) "
    hFlush stdout
    code <- getLine
    if code == "" then 
        return ()
    else do
        let program = parse code
        evalProgram program
        mainInteractive

mainTranspile :: String -> IO ()
mainTranspile path = do
    handle <- openFile path ReadMode
    code <- hGetContents handle
    let program = parse code
    putStrLn $ transpile program
    hClose handle

mainEval :: String -> IO ()
mainEval path = do
    handle <- openFile path ReadMode
    code <- hGetContents handle
    let program = parse code
    evalProgram program
    hClose handle

mainHelp :: IO ()
mainHelp = putStrLn $ unlines [
        "Haskell Brainfuck interpreter/transpiler.",
        "https://github.com/Washi1337",
        "",
        "  --interactive      Run in interactive mode.",
        "  --eval             Evaluate a brainfuck script.",
        "  --transpile        Transpile brainfuck program to C."
    ]

evalProgram :: BfProgram -> IO ()
evalProgram program = do
    let tape = boundedTape tapeSize :: Tape Int
    resultTape <- eval defaultIO tape program
    putStr "\nResulting tape: "
    putStrLn $ show $ contents $ resultTape


parse :: [Char] -> BfProgram
parse code = unpackParseResult result
    where
        result                      = parseBrainFuck code
        unpackParseResult (Left a)  = error $ show a
        unpackParseResult (Right a) = a