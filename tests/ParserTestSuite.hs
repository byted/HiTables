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
    testGroup "Parser" [
        testGroup "getAction"
        [
            testCase "simple" test_getAction,
            testCase "err" test_getActionErr,
            testCase "no action given" test_getActionErrMissing
        ],
        testGroup "parseIPtables"
        [
            testCase "empty input" test_parseIPtablesEmpty,
            testCase "is it stable?" test_parseIPtablesIsStable,
            testCase "just a single chain" test_parseIPtablesSingleChainType
        ],
        testGroup "parseRawRuleFromWords"
        [
            testCase "simple rule" test_parseRawRuleFromWordsDefault,
            testCase "complete rule" test_parseRawRuleFromWordsComplete,
            testCase "parse a comment" test_parseRawRuleFromWordsComment,
            testCase "parse empty line" test_parseRawRuleFromWordsEmpty,
            testCase "not a valid rule" test_parseRawRuleFromWordsError
        ],
        testGroup "subnetParser"
        [
            testCase "simple subnet string" test_subnetParser,
            testCase "no mask given" test_subnetParserNoMask,
            testCase "invalid mask" test_subnetParserErrMask,
            testCase "invalid IP" test_subnetParserErrIP
        ],
        testGroup "protocolParser"
        [
            testCase "simple protocl parse" test_protocolParserTcp,
            testCase "parse uppercase prot" test_protocolParserUDP,
            testCase "prot as legit number" test_protocolParserNbr,
            testCase "no input" test_protocolParserEmptyInput,
            testCase "invalid number" test_protocolParserBadNbr,
            testCase "invalid string" test_protocolParserBadString
        ],
        testGroup "portsParser"
        [
            testCase "simple single port" test_portsParserSinglePort,
            testCase "port range" test_portsParserRange,
            testCase "empty input" test_portsParser
        ],
        testGroup "subnetToInterval"
        [
            testCase "simple" test_subnetToInterval
        ],
        testGroup "getLegitRawRules"
        [
            testCase "simple filter" test_getLegitRawRules
        ],
        testGroup "toChainDict"
        [
            testCase "is stable?" test_toChainDictStable
        ]
    ]
    ]