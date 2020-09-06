module BfHs.Tape where

{-|
    The main tape data constructor.

    The constructor consists of three parts:
    - The left hand side of the data pointer (reversed)
    - The value at the current data pointer
    - THe right hand side of the data pointer.
-}
data Tape a = Tape [a] a [a]
    deriving (Show)

instance (Eq a) => Eq (Tape a) where
    (Tape l1 c1 r1) == (Tape l2 c2 r2) = (l1 == l2) && (c1 == c2) && (r1 == r2)

{-|
    Reads the cell from the tape at the current data pointer.
-}
readCell :: Tape a -> a
readCell (Tape _ cell _) = cell

{-|
    Writes a value into the cell on the tape at the current data pointer.
-}
writeCell :: Tape a -> a -> Tape a
writeCell (Tape left _ right) value = Tape left value right

{-|
    Moves the data pointer to the left.
-}
moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) cell right) = Tape ls l (cell:right)

{-|
    Moves the data pointer to the right.
-}
moveRight :: Tape a -> Tape a
moveRight (Tape left cell (r:rs)) = Tape (cell:left) r rs