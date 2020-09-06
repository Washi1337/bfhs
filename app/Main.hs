module Main where

import BfHs.Language ( parseBrainFuck )
import BfHs.Tape ( Tape, boundedTape, contents )
import BfHs.Interpreter ( eval, IODevice(IODevice) )

import Data.Either ( fromRight ) 
import Data.Char ( ord, chr )
import System.Environment ( getArgs ) 
import System.IO

tapeSize = 100

defaultIO :: IODevice IO Int
defaultIO = IODevice (writeToStdOutput) (readFromStdInput)
    where 
        writeToStdOutput x = putChar $ chr x
        readFromStdInput  = do 
            str <- getLine
            return $ ord $ head str

main :: IO ()
main = do
    args <- getArgs

    if (length args) == 0 then
        mainInteractive
    else 
        mainRunFile args

mainInteractive :: IO ()
mainInteractive = do 
    putStr "(fuck) "
    hFlush stdout
    code <- getLine
    if code == "" then 
        return ()
    else do
        evalString code
        mainInteractive

mainRunFile :: [String] -> IO ()
mainRunFile args = do
    handle <- openFile path ReadMode
    code <- hGetContents handle
    evalString code
    hClose handle  
    where 
        [path] = args

evalString :: String -> IO ()
evalString code = do
    let tape = boundedTape tapeSize :: Tape Int
    resultTape <- eval defaultIO tape (fromRight [] $ parseBrainFuck code)
    putStr "\nResulting tape:"
    putStrLn $ show $ contents $ resultTape
