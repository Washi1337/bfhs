import Test.HUnit
import BfHs.TapeTest

tests = TestList [tapeTests]

main :: IO Counts
main = runTestTT tests