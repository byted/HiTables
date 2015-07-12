{- Author: Stefan Selent -}
module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.HUnit
 
import IPtablesTransform.EmitterTest
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
    [
        testGroup "tree2List"
        [
            testCase "leaf node - only return rules in this node" test_tree2ListLeaf,
            testCase "small tree" test_tree2List
        ],
        testGroup "X2String"
        [
            testCase "rule2String" test_rule2String,
            testCase "rawRule2String" test_rawRule2String,
            testCase "rawRule2StringAgain" test_rawRule2StringAgain,
            testCase "box2String" test_box2String,
            testCase "box2String with wildcard ip src" test_box2StringPartlyRemoveIPWildcard,
            testCase "rawBox2StringSimple simple UDP rule" test_rawBox2StringSimple,
            testCase "rawBox2String" test_rawBox2String,
            testCase "rawBox2String only prot negated" test_rawBox2StringProtNeg,
            testCase "rawBox2String only ip src and it is negated" test_rawBox2StringIPsrcNeg
        ],
        testGroup "chainIt"
        [
            testCase "single node" test_chainItSingleNode,
            testCase "root with 1 child" test_chainItNodeWithOneChild,
            testCase "root with 2 children" test_chainItSmallTree,
            testCase "tree with 2 children and rules in those children" test_chainItSmallTreeWithRules,
            testCase "bigger tree with some rules" test_chainItTree2,
            testCase "right weighted tree" test_chainItRight,
            testCase "binary search integration test" test_chainItBinaryIntegration
        ],
        testGroup "linearChildSearch"
        [
            testCase "empty input" test_linearChildSearchEmpty,
            testCase "3 elements" test_linearChildSearch3
        ],
        testGroup "binaryChildSearch"
        [
            testCase "empty input" test_binaryChildSearchEmpty,
            testCase "single element - lin. search" test_binaryChildSearch1Elem,
            testCase "2 elements - lin. search" test_binaryChildSearch2Elem,
            testCase "4 - bin. search" test_binaryChildSearch4Elem,
            testCase "8 - bin. search" test_binaryChildSearch8Elem
        ]
    ]