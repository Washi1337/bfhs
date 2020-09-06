module BfHs.Interpreter 
    ( evalProgram )
    where 

import BfHs.Language ( BfProgram, BfAstNode(..) )
import BfHs.Tape ( moveLeft, moveRight, writeCell, Tape(cell) )
import Control.Monad ( foldM )

evalProgram :: (Eq a, Num a, Show a, Read a) => Tape a -> BfProgram -> IO (Tape a)
evalProgram state program = foldM evalNode state program

evalNode :: (Eq a, Num a, Show a, Read a) => Tape a -> BfAstNode -> IO (Tape a)
evalNode state node = case node of 
    
    MoveLeft -> return $ moveLeft state
    MoveRight -> return $ moveRight state
    Increment -> return $ writeCell state $ (cell state) + 1
    Decrement -> return $ writeCell state $ (cell state) - 1

    Output -> do 
        putStr $ show $ cell state
        return state

    Input -> do 
        valueString <- getLine
        return $ writeCell state (read valueString)

    Loop subProgram -> do
        if cell state == 0 then 
            return state
        else do 
            nextState <- evalProgram state subProgram
            evalNode nextState node