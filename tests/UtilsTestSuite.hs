{- Author: Stefan Selent -}
module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.HUnit
 
import IPtablesTransform.ParserTest
import IPtablesTransform.UtilsTest
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
    [
    testGroup "UtilsTest" [
        testGroup "toDict"
        [
            testCase "create dict from list" test_toDict,
            testCase "with negation" test_toDictNeg,
            testCase "empty input" test_toDictEmpty,
            testCase "list with no options" test_toDictNoOptions
        ],
        testGroup "isIntervalNegated"
        [
            testCase "is negated" test_isIntervalNegatedYes,
            testCase "not negated" test_isIntervalNegatedNo
        ],
        testGroup "checkIf2ByteSize"
        [
            testCase "upper bound" test_checkIf2ByteSizeUpper,
            testCase "lower bound" test_checkIf2ByteSizeLower,
            testCase "not" test_checkIf2ByteSizeNot
        ],
        testGroup "checkIfByteSize"
        [
            testCase "upper bound" test_checkIfByteSizeUpper,
            testCase "lower bound" test_checkIfByteSizeLower,
            testCase "not" test_checkIfByteSizeNot
        ],
        testGroup "replaceNth"
        [
            testCase "empty list" test_replaceNthEmpty,
            testCase "index < 0" test_replaceNthIndexTooLow,
            testCase "index > length list" test_replaceNthIndexTooHigh,
            testCase "simple case" test_replaceNth
        ],
        testGroup "splitIn3"
        [
            testCase "empty list" test_splitIn3Empty,
            testCase "1 element" test_splitIn3Single,
            testCase "2 elemts" test_splitIn3Two,
            testCase "even number of elements" test_splitIn3,
            testCase "odd number of elements" test_splitIn3Odd
        ]
    ]
    ]