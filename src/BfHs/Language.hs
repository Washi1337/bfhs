module BfHs.Language where

{-| 
    The data constructor representing an abstract syntax tree node in a brainfuck program.
-}
data BfAstNode 
    = MoveLeft          -- ^ Moves the data pointer to the left (<).
    | MoveRight         -- ^ Moves the data pointer to the right (>).
    | Increment         -- ^ Increments the value referenced by the data pointer by one (+)
    | Decrement         -- ^ Decrements the value referenced by the data pointer by one (-)
    | Output            -- ^ Writes the value referenced by the data pointer to the standard output (+).
    | Input             -- ^ Reads a single value from the standard input to the cell referenced by the data pointer.
    | Loop [BfAstNode]  -- ^ Describes a loop that repeats execution until the value referenced by the data pointer is 0 ([]).

{-|
    Defines a brainfuck program
-}
type BfProgram = [BfAstNode]