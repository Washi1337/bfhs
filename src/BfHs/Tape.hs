module BfHs.Tape where

{-|
    The main tape data constructor.
-}
data Tape a = Tape [a] a [a]
    deriving (Show)


moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) c rs) = Tape ls l (c:rs)
