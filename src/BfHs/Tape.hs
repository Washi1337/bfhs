module BfHs.Tape where

{-|
    The main tape data constructor.
-}
data Tape a = Tape {
    left  :: [a],    -- ^ The left hand side of the data pointer (reversed)
    cell  :: a,      -- ^ The value at the current data pointer
    right :: [a]     -- ^ The right hand side of the data pointer.
} deriving (Show)

instance (Eq a) => Eq (Tape a) where
    (Tape left1 cell1 right1) == (Tape left2 celleft2 right2) = 
        (left1 == left2) && 
        (cell1 == celleft2) && 
        (right1 == right2)

{-|
    Generates a tape instance from a list.
-}
tapeFromList :: [a] -> Tape a
tapeFromList []     = error "Tape requires at least one element."
tapeFromList [x]    = Tape [] x []
tapeFromList (x:xs) = Tape [] x xs

{-|
    Generates a tape initialized with a number of zero cells.
-}
boundedTape :: Num a => Int -> Tape a
boundedTape size = tapeFromList $ take size $ repeat 0

{- |
    Obtains the contents of the tape as a list.
-}
contents :: Tape a -> [a]
contents (Tape left cell right) = (reverse left) ++ [cell] ++ right

{-|
    Writes a value into the cell on the tape at the current data pointer.
-}
writeCell :: Tape a -> a -> Tape a
writeCell (Tape left _ right) value = Tape left value right

{-|
    When the tape contains numbers, increments the value of the current cell by one.
-}
incrementCell :: (Num a) => Tape a -> Tape a
incrementCell tape = writeCell tape $ (cell tape) + 1

{-|
    When the tape contains numbers, decrements the value of the current cell by one.
-}
decrementCell :: (Num a) => Tape a -> Tape a
decrementCell tape = writeCell tape $ (cell tape) - 1

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
