module BfHs.TapeTest where

import Test.HUnit
import BfHs.Tape

tapeTests :: Test
tapeTests = TestList [ 
        readCellTest,
        writeCellTest,
        moveLeftTest,
        moveRightTest
    ]

readCellTest :: Test
readCellTest = TestCase (assertEqual "Should read the current value." test expected)
    where 
        initialState = Tape [3,2,1] 4 [5,6,7]
        test         = readCell initialState
        expected     = 4

writeCellTest :: Test
writeCellTest = TestCase (assertEqual "Should replace the current value." test expected)
    where 
        initialState = Tape [3,2,1] 4 [5,6,7]
        newValue     = 10
        test         = writeCell initialState newValue
        expected     = Tape [3,2,1] newValue [5,6,7]

moveLeftTest :: Test
moveLeftTest = TestCase (assertEqual "Should move to the left." test expected)
    where 
        initialState = Tape [3,2,1] 4 [5,6,7]
        test         = moveLeft initialState
        expected     = Tape [2,1] 3 [4,5,6,7]

moveRightTest :: Test
moveRightTest = TestCase (assertEqual "Should move to the right." test expected)
    where 
        initialState = Tape [3,2,1] 4 [5,6,7]
        test         = moveRight initialState
        expected     = Tape [4,3,2,1] 5 [6,7]