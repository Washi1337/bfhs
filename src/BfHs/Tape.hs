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
    (Tape l1 c1 r1) == (Tape l2 c2 r2) = (l1 == l2) && (c1 == c2) && (r1 == r2)

boundedTape :: (RealFrac a1, Num a2) => a1 -> Tape a2
boundedTape size = Tape left cell right
    where left  = take (floor $ size/2) $ repeat 0
          cell  = 0
          right =take (ceiling $ size/2) $ repeat 0

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