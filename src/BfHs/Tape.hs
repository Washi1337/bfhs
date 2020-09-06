module BfHs.Tape where

{-|
    The main tape data constructor.
-}
data Tape a = Tape [a] a [a]
    deriving (Show)

instance (Eq a) => Eq (Tape a) where
    (Tape l1 c1 r1) == (Tape l2 c2 r2) = (l1 == l2) && (c1 == c2) && (r1 == r2)

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) c rs) = Tape ls l (c:rs)
