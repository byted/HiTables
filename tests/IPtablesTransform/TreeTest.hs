{- Author: Stefan Selent -}
module IPtablesTransform.TreeTest where

import Test.HUnit
import IPtablesTransform.Datatypes
import IPtablesTransform.Utils
import IPtablesTransform.Rule
import IPtablesTransform.Tree

{-
Helper functions
-}
treeRightWeighted r1 r2 r3 r4 = appendChildren defaultNode [appendRules defaultNode r1, b]
    where   b = appendChildren defaultNode [appendRules defaultNode r2, c]
            c = appendChildren defaultNode [appendRules defaultNode r3, appendRules defaultNode r4]

{-
Tests
-}

test_hasChildrenTrue = True @=? hasChildren (Node defaultBox [] [ defaultNode ])
test_hasChildrenFalse = False @=? hasChildren defaultNode
test_hasChildrenEmptyTree = False @=? hasChildren EmptyTree

test_appendRulesEmpty = defaultNode @=? appendRules defaultNode [] 
test_appendRulesEmptyTree = EmptyTree @=? appendRules EmptyTree [] 
test_appendRules = node @=? appendRules defaultNode [defaultRule]
    where   node = Node (defaultBox) [defaultRule]  []

test_replaceChildrenEmptyTree = EmptyTree @=? replaceChildren EmptyTree []
test_replaceChildrenEmpty = defaultNode @=? replaceChildren defaultNode []
test_replaceChildren = Node (defaultBox) [] [defaultNode] @=? replaceChildren defaultNode [defaultNode]

test_appendChildrenEmpty = defaultNode @=? appendChildren defaultNode [] 
test_appendChildrenEmptyTree = EmptyTree @=? appendChildren EmptyTree [] 
test_appendChildren = node @=? appendChildren defaultNode [defaultNode]
    where   node = Node (defaultBox) []  [defaultNode]

test_getStatsSingleNode = (1, 1, 1, 0) @=? getStats defaultNode
test_getStatsTwoNodesWRules = (2, 1, 2, 1) @=? (getStats $ appendChildren defaultNode [appendRules defaultNode [defaultRule]])
test_getStatsMoreThan1Child = (3,2,2,0) @=? (getStats $ appendChildren defaultNode [defaultNode, defaultNode])
test_getStatsDepth = (7,4,3,0) @=? (getStats $ appendChildren defaultNode [c1, c2])
    where   c1 = appendChildren defaultNode [defaultNode, defaultNode]
            c2 = appendChildren defaultNode [defaultNode, defaultNode]

test_purgeTree = res @=? (purgeTree $ treeRightWeighted [] [] [] [defaultRule])
    where   res = appendChildren defaultNode [b]
            b = appendChildren defaultNode [c]
            c = appendChildren defaultNode [appendRules defaultNode [defaultRule]]
test_purgeTreeNoMods = treeRightWeighted [r1] [r2] [r3] [r4] @=? (purgeTree $ treeRightWeighted [r1] [r2] [r3] [r4])
    where   r1 = r2
            r2 = r3
            r3 = r4
            r4 = defaultRule

test_npNoCut = 0 @=? np defaultNode
test_np = 1 @=? np (appendChildren defaultNode [defaultNode, defaultNode])