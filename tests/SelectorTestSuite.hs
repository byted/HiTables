module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.HUnit
 
import IPtablesTransform.SelectorTest
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
    [
        testGroup "getRelevantBlocks"
        [
            testCase "empty chain" test_getRelevantBlocksEmpty,
            testCase "all rules are relevant" test_getRelevantBlocksCompleteRelevant,
            testCase "mixed" test_getRelevantBlocks,
            testCase "a big block of unsuitable rules should not be recognized" test_getRelevantBlocksNotSuitableBlock
        ]
    ]