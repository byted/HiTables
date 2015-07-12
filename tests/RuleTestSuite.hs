{- Author: Stefan Selent -}
module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.HUnit
 
import IPtablesTransform.RuleTest
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
    [
    testGroup "RulesTest" [
        testGroup "isProtocolNegated"
        [
            testCase "yes" test_isProtocolNegated,
            testCase "no" test_isProtocolNegatedFalse
        ],
        testGroup "isProtocol"
        [
            testCase "check for UDP true" test_isUDP,
            testCase "check for UDP false" test_isUDPFalse,
            testCase "check for TCP true" test_isTCP,
            testCase "check for TCP false" test_isTCPFalse
        ],
        testGroup "boxesTouching" 
        [
            testCase "simple test with default rules" test_boxesTouching,
            testCase "two boxes not touching in dim 2" test_boxesTouchingNotTouching,
            testCase "boxes touching in a single point in dim 0" test_boxesTouchingBorderCase
        ],
        testGroup "ruleNormalizer" 
        [
            testCase "no negated dimensions" test_ruleNormalizer,
            testCase "only negated dimensions" test_ruleNormalizerAllNeg,
            testCase "one negated dimension" test_ruleNormalizerOneNeg,
            testCase "insane rule" test_ruleNormalizerInsane
        ],
        testGroup ""
        [
            testCase "get protocol dimension" test_getIntervalByIndexProt,
            testCase "get IP Src dimension" test_getIntervalByIndexIPSrc,
            testCase " get port src dimension" test_getIntervalByIndexPortSrc
        ],
        testGroup "rangeFromInterval"
        [
            testCase "simple interval" test_rangeFromInterval,
            testCase "overflow / inverted interval - won't handle" test_rangeFromIntervalBad
        ],
        testGroup "word32ToIP"
        [
            testCase "null ip" test_word32ToIPNull,
            testCase "together with <subnetToInterval>" test_word32ToIP
        ],
        testGroup "interval2X"
        [
            testCase "interval2IPrange" test_interval2IPrange,
            testCase "interval2Range" test_interval2Range
        ],
        testGroup "static rule optimizations"
        [
            testCase "remove redundant rules" test_removeRedundantRules,
            testCase "remove shadowed rules" test_removeShadowedRules,
            testCase "removeShadowing" test_removeShadowing1,
            testCase "removeShadowing" test_removeShadowing2,
            testCase "removeShadowing" test_removeShadowing3,
            testCase "brutal" test_removeShadowedRulesBrutal
        ],
        testGroup "rawInterval2Protocol"
        [
            testCase "rawInterval2Protocol UDP" test_rawInterval2Protocol
        ]
    ]
    ]