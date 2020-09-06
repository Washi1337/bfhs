module BfHs.TapeTest where

import Test.HUnit
import BfHs.Tape

tapeTests :: Test
tapeTests = TestList [ 
        moveLeftTest 
    ]

moveLeftTest :: Test
moveLeftTest = TestCase (assertEqual "Initial test" test expected)
    where 
        initial     = Tape [3,2,1] 4 [5,6,7] :: Tape Int
        test        = moveLeft initial
        expected    = Tape [2,1] 3 [4,5,6,7]