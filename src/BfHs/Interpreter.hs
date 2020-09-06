module BfHs.Interpreter 
    ( eval,
      IOInterface (..) )
    where 

import BfHs.Language ( 
    BfProgram,
     BfAstNode(..) )

import BfHs.Tape
    ( Tape(cell),
      writeCell,
      moveLeft,
      moveRight,
      incrementCell,
      decrementCell )

import Control.Monad (
     foldM )

{-|
    Provides a contract that allows the brainfuck interpreter to read and write values from a device or file.
-}
data IOInterface m a = IOInterface { 
    writeValue :: a -> m (),
    readValue :: m a
}

{-|
    Represents the program execution state.
-}
data ProgramState m a = ProgramState {
    io :: IOInterface m a,
    tape :: Tape a
}

{-|
    Transforms the tape inside a program state according to the provided tape transformation function.
-}
transformTape :: ProgramState m a    -- ^ The program state that contains the tape to be transformed.
              -> (Tape a -> Tape a)  -- ^ The function to apply to the tape.
              -> ProgramState m a    -- ^ The new program state.
transformTape state f = ProgramState (io state) (f $ tape state)

{-|
    Evaluates a brainfuck program with the provided initial state and input/output device.
-}
eval :: (Monad m, Eq a, Num a, Show a, Read a) 
     => IOInterface m a     -- ^ The device to use for interacting with the outside world.
     -> Tape a              -- ^ The initial state of the tape.
     -> BfProgram           -- ^ The program to evaluate.
     -> m (Tape a)          -- ^ The final state of the tape.
eval io initialTape program = do 
        finalState <- evalProgram initialState program 
        return $ tape finalState
    where
        initialState = ProgramState io initialTape

{-|
    Evaluates a brainfuck program.
-}
evalProgram :: (Monad m, Eq a, Num a, Show a, Read a) 
            => ProgramState m a             -- ^ The current state of the interpreter.
            -> BfProgram                    -- ^ The program to evaluate.
            -> m (ProgramState m a)         -- ^ The resulting program state.
evalProgram state program = foldM evalNode state program

{- |
    Evaluates a single AST node in a brainfuck program.
-}
evalNode :: (Monad m, Eq a, Num a, Show a, Read a) 
         => ProgramState m a        -- ^ The current state of the interpreter.
         -> BfAstNode               -- ^ The AST node to evaluate.
         -> m (ProgramState m a)    -- ^ The resulting program state.
evalNode state node = case node of 
    
    MoveLeft -> return $ transformTape state $ moveLeft
    MoveRight -> return $ transformTape state $ moveRight
    Increment -> return $ transformTape state $ incrementCell
    Decrement -> return $ transformTape state $ decrementCell

    Output -> do 
        writeValue (io state) (cell $ tape state)
        return state

    Input -> do 
        value <- readValue (io state)
        return $ transformTape state $ (\tape -> writeCell tape value)

    Loop subProgram -> do
        if cell (tape state) == 0 then 
            return state
        else do 
            nextState <- evalProgram state subProgram
            evalNode nextState node
