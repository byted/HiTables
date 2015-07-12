module IPtablesTransform.TransformationEngineTest where

import Test.HUnit
import Data.List as L
import Data.Word
import Data.HashSet as H

import IPtablesTransform.Datatypes
import IPtablesTransform.TransformationEngine
import IPtablesTransform.Tree
import IPtablesTransform.Rule
import IPtablesTransform.Utils
import IPtablesTransform.Parser


binth = 4
spfac = 2
spmf n = spfac * n

generateUDPRawRules :: Int -> [RawRule]
generateUDPRawRules n = L.foldr (\x acc -> (udpRule x):acc) [] [0..(n-1)]
    where   udpRule x = RawRule(RawBox (False, univS) (False, univD) (False, univSP) (False, (asWord x, asWord x)) (False,(17, 17))) (Chain "DROP")

generateUDPRules :: Int -> [Rule]
generateUDPRules n = L.foldr (\x acc -> (udpRule x):acc) [] [0..(n-1)]
    where   udpRule x = Rule (Box univS univD univSP (asWord x, asWord x)(17, 17)) (Chain "DROP")


test_buildHiCutsTreeEmpty = EmptyTree @=? buildHiCutsTree [] 1 1 MinSM
test_buildHiCutsLessThanBinth = Node tcpBox [defaultRule] [] @=? buildHiCutsTree [defaultRawRule] 1 1 MaxDistinctRules
test_buildHiCutsTree = res @=? buildHiCutsTree rules 3 2 MinMaxRulesPerChild
    where
        rules = [   RawRule (RawBox (False,(125,125)) (False,univD) (False,univSP) (False,univDP) (False,(6,6))) (Chain "ACCEPT")
                ,   RawRule (RawBox (False,(1000000000,1000000000)) (False,univD) (False,univSP) (False,univDP) (False,(6,6))) (Chain "ACCEPT")
                ,   RawRule (RawBox (False,(1000000000,1000000000)) (False,univD) (False,univSP) (False,univDP) (False,(6,6))) (Chain "ACCEPT")
                ,   RawRule (RawBox (False,(2000000000,2000000000)) (True,(1,1)) (False,univSP) (False,univDP) (False,(6,6))) (Chain "ACCEPT")
                ,   RawRule (RawBox (False,(125,125)) (False,(22,22)) (False,univSP) (False,univDP) (False,(6,6))) (Chain "ACCEPT")
                ]
        res = purgeTree $ getBestCut (Node tcpBox (removeRedundantRules . removeShadowedRules $ concatMap ruleNormalizer rules) []) 0 spmf

test_buildHiCutsTreeUDP = res @=? buildHiCutsTree rawRules binth spfac MaxDistinctRules
    where   rawRules = generateUDPRawRules 10
            rules = generateUDPRules 10
            distRules = sortRulesByIsDistinct rules
            root = Node udpBox (rules) []
            res = purgeTree $ transformNode binth spmf (getDimAlgo MaxDistinctRules) distRules 0 root

{-
                A
         ______/ \
        /         \
       R1          B
                ___/\___    
               /        \   
              R2         C
                     ___/\___    
                    /        \   
                   R3         R4
-}

test_newIntervals = [(50,75),(76,100),(101,125),(126,150)] @=? newIntervals (50,150) 3
test_newIntervals0Cuts = [] @=? newIntervals (50,150) 0


{- helper -}
getXRules x = unfoldr (\x -> if x > 0 then Just (defaultRule, x-1) else Nothing) x
getNodeWithXRules x = Node defaultBox (getXRules x) []




test_cutEmptyTree = EmptyTree @=? cut EmptyTree 3 2
test_cutHasChildren = cut defaultNode 3 2 @=? cut n1 3 2
    where   n1 = Node defaultBox [] [defaultNode]

test_cutOrderOfChildren = ([r1], [r2]) @=? (rules c1, rules c2)
    where   n1 = Node defaultBox [r1, r2] []
            r1 = Rule (Box univS univD univSP (0,0) (6,6)) (Chain "DROP")
            r2 = Rule (Box univS univD univSP (snd univDP, snd univDP) (6,6)) (Chain "DROP")
            [c1, c2] = children $ cut n1 3 1


-- we've only default rules → after cut _all_ resulting nodes contain _all_ rules 
test_cutBinary = resNode @=? cut node 0 numOfCuts
    where   node = getNodeWithXRules (binth + 1)
            resNode = appendChildren defaultNode [c1, c2]
            c1 = Node f1 (rules node) []
            c2 = Node f2 (rules node) []
            f1 = Box (head newIv) univD univSP univDP univP
            f2 = Box (newIv !! 1) univD univSP univDP univP
            newIv = newIntervals univS numOfCuts
            numOfCuts = 1


test_modifyBoxDim = resBox @=? modifyBoxDim defaultBox (0,0) 0
    where   resBox = Box (0,0) univD univSP univDP univP


test_categorizeRulesNoRule = [defaultNode] @=? categorizeRules [] 0 [defaultNode]
test_categorizeRulesNoNodes = [] @=? categorizeRules [defaultRule] 0 []
test_categorizeRulesSingleRuleSingleNode = [resNode] @=? categorizeRules [defaultRule] 3 [defaultNode]
    where   resNode = Node defaultBox [defaultRule] []
test_categorizeRulesSingleRule = [resNode, resNode] @=? categorizeRules [defaultRule] 3 [defaultNode, defaultNode]
    where   resNode = Node defaultBox [defaultRule] []
test_categorizeOneNotMatching = [resNode] @=? categorizeRules [r1, r2] 4 [n1]
    where   n1 = Node (Box univS univD univSP univDP (0,20)) [] []
            r1 = Rule (Box univS univD univSP univDP (0,20)) (Chain "DROP")
            r2 = Rule (Box univS univD univSP univDP (21,21)) (Chain "DROP")
            resNode = appendRules n1 [r1]

test_smEmpty = 0 @=? sm EmptyTree
test_smNoRulesInChildren = np node @=? sm node
    where   node = appendChildren defaultNode [defaultNode, defaultNode]
test_smNoCuts = 0 @=? sm (appendRules defaultNode [defaultRule])
test_sm = 5 @=? sm (cut (appendRules defaultNode [defaultRule, defaultRule]) 0 1)

test_getBestCut =
    cut node 0 4 @=? getBestCut node 0 spmf
        where   node = appendRules defaultNode . take 5 . repeat $ defaultRule
test_getBestCut8 =
    cut node dimToCut 8 @=? getBestCut node dimToCut spmf
        where   node = appendRules defaultNode modRules
                modRules = take 5 . repeat $ Rule (Box univS univD univSP univDP (0,0)) (Chain "DROP")
                dimToCut = 4

test_getBestCutAvoidDeadlock = 
    getBestCut node dimToCut spmf @?= cut node dimToCut 4
        where   node = Node     (Box univS univD univSP (0,4) univP)   -- box
                                (generateUDPRules (binth + 1))          -- generate rules
                                []                                      -- no children
                dimToCut = 3


{-
If rule r1 gets cut in any other dimension than 0 or 3, all resulting nodes contain binth + 1 rules.
In 0, one node contains binth+1 r1 and binth+2 r2 rules, other nodes are empty
In 3, one node contains binth+2 r2 and binth+1 r1, other nodes contain binth+1 r1 rules
-}
test_minMaxRulesPerChild = getBestCut node 0 spmf @=? minMaxRulesPerChild node spmf H.empty
    where   node = appendRules defaultNode $ r1List ++ r2List
            r1 = Rule (Box (0,4) univD univSP univDP univP) (Chain "DROP")
            r2 = Rule (Box (0,4) univD univSP (0,4) univP) (Chain "ACCEPT")
            getXRules x r = take x $ repeat r
            r1List =  getXRules (binth) r1
            r2List = getXRules (binth) r2

test_minMaxRulesPerChildTooSmall = node @=? minMaxRulesPerChild node spmf H.empty
    where   node = Node (Box (2, 2) (3, 4) (100, 100) (44, 44) (17, 17)) [r1, r2] []
            r1 = Rule (Box (2, 2) (3, 3) (100, 100) (44, 44) (17, 17)) (Chain "DROP")
            r2 = Rule (Box (2, 2) (4, 4) (100, 100) (44, 44) (17, 17)) (Chain "ACCEPT")

{-
Cutting 0 leads to one node with all rules and all other nodes with none.
Cutting any other dimension leads to every rule in every node.
=> sm of cut in 0 is smaller than sm of other dimensions
-}
test_minSmDim = getBestCut node 0 spmf @=? minSmDim node spmf H.empty
    where   node = appendRules defaultNode r1List
            r1List = take (binth+1) . repeat $ Rule (Box (0,4) univD univSP univDP univP) (Chain "DROP")

test_minSmDimTooSmall = node @=? minSmDim node spmf H.empty
    where   node = Node (Box (2, 2) (3, 4) (100, 100) (44, 44) (17, 17)) [r1, r2] []
            r1 = Rule (Box (2, 2) (3, 3) (100, 100) (44, 44) (17, 17)) (Chain "DROP")
            r2 = Rule (Box (2, 2) (4, 4) (100, 100) (44, 44) (17, 17)) (Chain "ACCEPT")



maxDistinctRuleComponentsRuleset = 
    [   Rule (Box (0,1) univD univSP univDP (0,1)) (Chain "DROP")
    ,   Rule (Box univS univD univSP univDP (1,1)) (Chain "DROP")
    ,   Rule (Box univS univD univSP univDP (5,15)) (Chain "DROP")
    ,   Rule (Box (0,4) univD univSP univDP (25,144)) (Chain "DROP")
    ]
maxDistinctRuleComponentsRuleset2 = 
    [   Rule (Box univS univD univSP (0,0) univP) (Chain "DROP")
    ,   Rule (Box univS univD univSP (1,1) univP) (Chain "DROP")
    ,   Rule (Box univS univD univSP (2,2) univP) (Chain "DROP")
    ,   Rule (Box univS univD univSP (3,3) univP) (Chain "DROP")
    ]

{-
Protocol dimension has 2 completely distinct components, other dimensions have zero
-}
test_maxDistinctRuleComponents = getBestCut node 3 spmf @=? maxDistinctRuleComponents node spmf distRules
    where   node = appendRules defaultNode maxDistinctRuleComponentsRuleset2
            distRules = sortRulesByIsDistinct maxDistinctRuleComponentsRuleset2




countDistinctBoxesBoxset =
    [   Box (0,1) univD univSP (6,256) (0,0)
    ,   Box (4,7) univD univSP (6,256) (1,2)
    ,   Box (9,12) univD univSP (6,256) (4,8)
    ,   Box (13,13) univD univSP (6,256) (17,35)
    ,   Box (13,2555) univD univSP (6,256) (45,67)
    ,   Box (125,126) univD univSP (1,5) (112,112)
    ,   Box (125,2^15) univD univSP (6,256) (156,255)
    ]
countDistinctBoxesHashSet = sortRulesByIsDistinct $ L.map (\b -> Rule b (Chain "DROP")) countDistinctBoxesBoxset

test_countDistinctBoxesIPSrc = 3 @=? countDistinctBoxes countDistinctBoxesBoxset countDistinctBoxesHashSet 0
test_countDistinctBoxesIPDst = 0 @=? countDistinctBoxes countDistinctBoxesBoxset countDistinctBoxesHashSet 1
test_countDistinctBoxesPortDst = 1 @=? countDistinctBoxes countDistinctBoxesBoxset countDistinctBoxesHashSet 3
test_countDistinctBoxesProtocol = 7 @=? countDistinctBoxes countDistinctBoxesBoxset countDistinctBoxesHashSet 4



-- maximum is 4 atm
test_getTestCutsEverythingTooSmall = [] @=? getTestCuts n1 spmf
    where   n1 = Node (Box (0,0) (1,1) (3,6) (0,1) (33,34)) [] []
test_getTestCuts = res @=? getTestCuts defaultNode spmf
    where   res =   [   getBestCut defaultNode 0 spmf 
                    ,   getBestCut defaultNode 1 spmf 
                    ,   getBestCut defaultNode 2 spmf 
                    ,   getBestCut defaultNode 3 spmf 
                    ,   getBestCut defaultNode 4 spmf 
                    ]
test_getTestCutsProt = res @=? getTestCuts n1 spmf
    where   n1 = Node (Box univS univD univSP univDP (33,34)) [] []
            res =   [   getBestCut n1 0 spmf 
                    ,   getBestCut n1 1 spmf 
                    ,   getBestCut n1 2 spmf 
                    ,   getBestCut n1 3 spmf 
                    ]

test_convertSimple = res @=? (fst $ convert input 3 2 MaxDistinctRules Linear)
    where   res = 
                "-A INPUT -p 0 --sport 0:65535 --dport 0:65535 -j ACCEPT\n" ++
                "-A INPUT -m iprange --src-range 192.168.1.2-192.168.1.2 -p 0 --sport 0:65535 --dport 0:65535 -j ACCEPT\n" ++
                "-A INPUT ! -p 17 --sport 0:65535 --dport 21:21 -j DROP\n" ++
                "-A OUTPUT -m iprange ! --dst-range 200.0.0.0-200.255.255.255 -p 6 --sport 0:65535 --dport 0:65535 -j DROP\n" ++
                "-A OUTPUT -p 0 --sport 0:65535 ! --dport 21:21 -j LOG\n"
            Right input = parseIPtables $   "-A INPUT -j ACCEPT\n -A INPUT -s 192.168.1.2/32 -d 0.0.0.0/0 -j ACCEPT\n -A INPUT ! -p udp --dport 21 -j DROP\n" ++
                                            "-A OUTPUT -p TCP ! -d 200.0.0.1/8 -j DROP\n -A OUTPUT ! --dport 21 -j LOG"