import Test.HUnit

test1 :: Test
test1 = TestCase $ assertEqual "sample test" (2 :: Integer) 3

tests :: Test
tests = TestList [
    TestLabel "test1" test1
  ]

main :: IO ()
main = runTestTTAndExit tests
