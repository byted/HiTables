module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.HUnit
 
import IPtablesTransform.TransformationEngineTest
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
    [
        testGroup "buildHiCutsTree"
        [
            testCase "no rules as input" test_buildHiCutsTreeEmpty,
            testCase "small tree" test_buildHiCutsTree,
            testCase "root node with num of rules lower than binth should not be changed" test_buildHiCutsLessThanBinth,
            testCase "list of rules with ascending udp dst port" test_buildHiCutsTreeUDP
        ],
        testGroup "newIntervals"
        [
            testCase "new intervals" test_newIntervals,
            testCase "zero cuts" test_newIntervals0Cuts
        ],
        testGroup "modifyBoxDim"
        [
            testCase "simple replacement" test_modifyBoxDim
        ],
        testGroup "categorizeRules"
        [
            testCase "no rules to categorize" test_categorizeRulesNoRule,
            testCase "no nodes as categories given" test_categorizeRulesNoNodes,
            testCase "single rule suitable for the only given node" test_categorizeRulesSingleRuleSingleNode,
            testCase "checking two rules, one to append, one to discard" test_categorizeOneNotMatching,
            testCase "rule should be appended to multiple nodes if suitable" test_categorizeRulesSingleRule
        ],
        testGroup "cut"
        [
            testCase "empty tree as input" test_cutEmptyTree,
            testCase "if node has children, they'll be overwritten" test_cutHasChildren,
            testCase "leftmost child has the lowest interval" test_cutOrderOfChildren,
            testCase "binary cut" test_cutBinary
        ],
        testGroup "sm"
        [
            testCase "empty tree" test_smEmpty,
            testCase "the node has no children" test_smNoRulesInChildren,
            testCase "the node wasn't cut" test_smNoCuts,
            testCase "node with two defaultRules should have sm of 5" test_sm
        ],
        testGroup "getBestCut"
        [
            testCase "cut 4 times" test_getBestCut,
            testCase "cut 8 times" test_getBestCut8,
            testCase "avoid deadlock if interval is too small to reach spmf limit" test_getBestCutAvoidDeadlock
        ],
        testGroup "minMaxRulesPerChild"
        [
            testCase "choose IPSrc" test_minMaxRulesPerChild,
            testCase "all intervals are too small" test_minMaxRulesPerChildTooSmall
        ],
        testGroup "minSmDim"
        [
            testCase "choose IPSrc" test_minSmDim,
            testCase "all intervals are too small" test_minSmDimTooSmall
        ],
        testGroup "maxDistinctRuleComponents"
        [
            testCase "choose Protocol" test_maxDistinctRuleComponents
        ],
        testGroup "countDistinctBoxes"
        [
            testCase "IPsrc 3 distinct" test_countDistinctBoxesIPSrc,
            testCase "IPDst 0 distinct" test_countDistinctBoxesIPDst,
            testCase "PortDst 1 distinct" test_countDistinctBoxesPortDst,
            testCase "Protocol 7 dst" test_countDistinctBoxesProtocol
        ],
        testGroup "getTestCuts"
        [
            testCase "all cimensions are too small" test_getTestCutsEverythingTooSmall,
            testCase "cut a default node in every dimension" test_getTestCuts,
            testCase "cut node in every dimension but protocol" test_getTestCutsProt
        ],
        testGroup "convert"
        [
            testCase "simple example without triggering optimization algorithm" test_convertSimple
        ]
    ]