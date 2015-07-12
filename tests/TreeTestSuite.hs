{- Author: Stefan Selent -}
module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.HUnit
 
import IPtablesTransform.TreeTest
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
    [
    testGroup "TreeTest" [
        testGroup "hasChildren"
        [
            testCase "children" test_hasChildrenTrue,
            testCase "no children" test_hasChildrenFalse,
            testCase "EmptyTree input" test_hasChildrenEmptyTree
        ],
        testGroup "appendRules"
        [
            testCase "Empty set of rules as input" test_appendRulesEmpty,
            testCase "EmptyTree as input" test_appendRulesEmptyTree, 
            testCase "append a single rule" test_appendRules
        ],
        testGroup "replaceChildren"
        [
            testCase "EmptyTree" test_replaceChildrenEmptyTree,
            testCase "Empty children" test_replaceChildrenEmpty,
            testCase "append single child" test_replaceChildren
        ],
        testGroup "appendChildren"
        [
            testCase "Empty set of children as input" test_appendChildrenEmpty,
            testCase "EmptyTree as input" test_appendChildrenEmptyTree, 
            testCase "append a single rule" test_appendChildren
        ],
        testGroup "getStats"
        [
            testCase "stats for a tree consiting of a singe node" test_getStatsSingleNode,
            testCase "stats for root with one child that has a rule" test_getStatsTwoNodesWRules,
            testCase "stats for a node with more than 1 child" test_getStatsMoreThan1Child,
            testCase "depth test" test_getStatsDepth
        ],
        testGroup "purgeTree"
        [
            testCase "purge simple tree" test_purgeTree,
            testCase "do not touch a tree where every leaf has at least one rule" test_purgeTreeNoMods
        ],
        testGroup "np"
        [
            testCase "no children - no cuts" test_npNoCut,
            testCase "single cut" test_np
        ]   
    ]
    ]