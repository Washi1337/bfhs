import Test.HUnit
import BfHs.TapeTest

tests = TestList [test1]

main :: IO Counts
main = runTestTT tests