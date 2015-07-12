module IPtablesTransform.EmitterTest where

import Test.HUnit

import IPtablesTransform.Datatypes
import IPtablesTransform.Parser (subnetParser)
import IPtablesTransform.Emitter
import IPtablesTransform.Rule
import IPtablesTransform.Tree
import IPtablesTransform.Parser (parseIPtables)


    --Example tree:
    --            A
    --     ______/ \_____
    --    /              \
    --   B               R4
    --  / \_____C
    -- /       / \
    --R1      /   \
    --       R2   R3

tree2 r1 r2 r3 r4 = appendChildren defaultNode a
    where   a = [appendChildren defaultNode b, Node (Box univS univD univSP univDP (6,6)) r4 []]
                where   b = [appendRules defaultNode r1, appendChildren defaultNode c]
                            where   c = [appendRules defaultNode r2, appendRules defaultNode r3]

    --Example tree:
       --         A
       --  ______/ \_____
       -- /    |       | \
       --R1     C       D  R5
       --  ___/\___    |
       -- /        \   R4
       --R2        R3
tree4 = appendChildren defaultNode c0
    where   c0 = [appendChildren defaultNode c1, appendChildren defaultNode c2, appendChildren defaultNode c3, appendChildren defaultNode c4]
                where   c1 = [] 
                        c2 = [appendChildren defaultNode c5, appendChildren defaultNode c6]
                            where   c5 = []
                                    c6 = []
                        c3 = []
                        c4 = [appendChildren defaultNode c7]
                            where   c7 = []
 --               A
 --        ______/  
 --       /    |   
 --      B     C    

treeSimple r1x r2x = appendChildren defaultNode c0
    where   c0 = [appendRules defaultNode r1x, appendRules defaultNode r2x]

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

treeRightWeighted r1 r2 r3 r4 = appendChildren defaultNode [appendRules defaultNode r1, b]
    where   b = appendChildren defaultNode [appendRules defaultNode r2, c]
            c = appendChildren defaultNode [appendRules defaultNode r3, appendRules defaultNode r4]



test_tree2ListLeaf = out @=? tree2List "C" "" Linear (appendRules defaultNode . take 5 $ repeat defaultRule)
    where   out = ["", ""] ++ (take 5 . repeat $ rule2String "C" defaultRule)


test_tree2List = out @=? tree2List "C" "" Linear (treeRightWeighted r1x r2x r3x r4x)
    where   r1x = [r1,r2,r3]   
            r2x = [r2,r4]
            r3x = [r3,r1,r2]
            r4x = [r4,r4,r4]
            out =   [   "-N C1\n"
                    ,   "-N C2\n"
                    ,   "-N C3\n","",""]  
                    ++  [ rule2String "C" (Rule defaultBox (Chain "C1"))
                        , rule2String "C1" (Rule defaultBox (Chain "C2"))
                        , rule2String "C2" (Rule defaultBox (Chain "C3"))] 
                                            ++ map (rule2String "C") r1x 
                                            ++ map (rule2String "C1") r2x 
                                            ++ map (rule2String "C2") r3x
                                            ++ map (rule2String "C3") r4x
                                            ++ ["-A C1 -j ACCEPT\n", "-A C2 -j ACCEPT\n", "-A C3 -j ACCEPT\n"]




r1 = Rule defaultBox (Chain "R1")
r2 = Rule defaultBox (Chain "R2")
r3 = Rule defaultBox (Chain "R3")
r4 = Rule defaultBox (Chain "R4")

test_chainItSingleNode = (0, [], []) @=? chainIt defaultNode 0 0 Linear
test_chainItNodeWithOneChild = (0, [], []) @=? chainIt (appendChildren defaultNode [defaultNode]) 0 0 Linear

test_chainItSmallTree = (1, [("0", Rule defaultBox (Chain "1"))], []) @=? chainIt (treeSimple [] []) 0 0 Linear

test_chainItSmallTreeWithRules = res @=? chainIt (treeSimple [r1, r2, r3] [r4]) 0 0 Linear
    where   res =   (1, [("0", Rule defaultBox (Chain "1"))], [   ("0", r1)
                                                            ,   ("0", r2)
                                                            ,   ("0", r3)
                                                            ,   ("1", r4)
                                                            ]
                    )


test_chainItTree2 = res @=? chainIt (tree2 [r1] [r2] [r3] [r4]) 0 0 Linear
    where   res =   (3, [("0", Rule (Box univS univD univSP univDP (6,6)) (Chain "3")), ("0", Rule defaultBox (Chain "1")), ("1", Rule defaultBox (Chain "2"))],
                        [   ("0", r1)
                        ,   ("1", r2)
                        ,   ("2", r3)
                        ,   ("3", r4)
                        ]
                    )

test_chainItRight = res @=? chainIt (treeRightWeighted [r1,r2,r3] [r2,r4] [r3,r1,r2] [r4,r4,r4]) 0 0 Linear
    where   res =   (3, [("0", Rule defaultBox $ Chain "1"), ("1", Rule defaultBox $ Chain "2"), ("2", Rule defaultBox $ Chain "3")],
                        [   ("0", r1), ("0", r2), ("0", r3)
                        ,   ("1", r2), ("1", r4)
                        ,   ("2", r3), ("2", r1), ("2", r2)
                        ,   ("3", r4), ("3", r4), ("3", r4)
                        ]
                    ) 


test_chainItBinaryIntegration = res @=? chainIt treeBinIntegration 0 0 Binary
    where   res =   (7, [   ("0", Rule b13 $ Chain "6"),
                            ("0", Rule b12 $ Chain "5"),
                            ("0", Rule b14 $ Chain "7"),
                            ("0", Rule b24 $ Chain "3"),
                            ("0", Rule (mergeBoxes b22 b23) $ Chain "0-1-0"),
                            ("0", Rule b25 $ Chain "4"),
                            ("0-1-0", Rule b22 $ Chain "1"),
                            ("0-1-0", Rule b23 $ Chain "2")
                        ],[] 
                    )
            treeBinIntegration = Node defaultBox [] [c11, Node b12 [] [], Node b13 [] [], Node b14 [] []]
            c11 = Node b11 [] [Node b21 [] [], Node b22 [] [], Node b23 [] [], Node b24 [] [], Node b25 [] []]
            b11 = Box univS (6,5000) univSP univDP (7,7)
            b12 = Box univS (5001,50000) univSP univDP (7,7)
            b13 = Box univS (50001,60000) univSP univDP (7,7)
            b14 = Box univS (60001,110000) univSP univDP (7,7)
            b21 = Box univS (6,5000) univSP (0,1) (7,7)
            b22 = Box univS (6,5000) univSP (2,100) (7,7)
            b23 = Box univS (6,5000) univSP (101,159) (7,7)
            b24 = Box univS (6,5000) univSP (160,16000) (7,7)
            b25 = Box univS (6,5000) univSP (16001,17000) (7,7)



test_linearChildSearchEmpty = [] @=? linearChildSearch "0" []
test_linearChildSearch3 = out @=? linearChildSearch "0" [(1, b1), (3, b2), (4, b3)]
    where   b1 = Box univS univD univSP (0, 25) (7,7)
            b2 = Box univS univD univSP (26, 30) (7,7)
            b3 = Box univS univD univSP (31, 13000) (7,7)  
            out =   [   ("0", Rule b1 $ Chain "1")
                    ,   ("0", Rule b2 $ Chain "3")
                    ,   ("0", Rule b3 $ Chain "4")  ]


test_binaryChildSearchEmpty = [] @=? binaryChildSearch "0" "0" []
test_binaryChildSearch1Elem = [("0", Rule defaultBox $ Chain "1")] @=? binaryChildSearch "0" "0" [(1, defaultBox)]
test_binaryChildSearch2Elem =   [   ("0", Rule b1 $ Chain "1")
                                ,   ("0", Rule b2 $ Chain "3")    ] @=? binaryChildSearch "0" "0" [(1, b1), (3, b2)]
    where   b1 = Box univS univD univSP (0, 25) (7,7)
            b2 = Box univS univD univSP (26, snd univDP) (7,7)

test_binaryChildSearch4Elem =  
    firstStep ++ leftStep  @=? binaryChildSearch "0" "0" [(1, b1), (3, b2), (4, b3), (10, b4)]
        where   b1 = Box univS univD univSP (0, 25) (7,7)
                b2 = Box univS univD univSP (26, 30) (7,7)
                b3 = Box univS univD univSP (31, 13000) (7,7)
                b4 = Box univS univD univSP (13001, snd univDP) (7,7)
                firstStep = [   ("0", Rule b3 $ Chain "4")
                            ,   ("0", Rule (mergeBoxes b1 b2) $ Chain "0-0-0")
                            ,   ("0", Rule b4 $ Chain "10")  ]
                leftStep = linearChildSearch "0-0-0" [(1, b1), (3, b2)]

test_binaryChildSearch8Elem = 
    firstStep ++ leftStep ++ rightStep @=? binaryChildSearch "0" "0" [(1,b1), (5,b2), (7,b3), (9,b4), (11,b5), (78,b6), (123,b7), (43367,b8)]
    where   b1 = Box univS univD univSP (0, 25) (7,7)
            b2 = Box univS univD univSP (26, 30) (7,7)
            b3 = Box univS univD univSP (31, 13000) (7,7)
            b4 = Box univS univD univSP (13001, 13002) (7,7)
            b5 = Box univS univD univSP (13003, 15000) (7,7)
            b6 = Box univS univD univSP (15001, 19000) (7,7)
            b7 = Box univS univD univSP (19001, 40000) (7,7)
            b8 = Box univS univD univSP (40001, snd univDP) (7,7)
            firstStep = [   ("0", Rule b5 $ Chain "11")
                        ,   ("0", Rule (mergeBoxList [b1,b2,b3,b4]) $ Chain "0-0-0")
                        ,   ("0", Rule (mergeBoxList [b6,b7,b8]) $ Chain "0-0-1")   ]
            leftStep =  snd $ binaryChildSearchRec "0-0-0" "0-0-" 2 [(1,b1), (5,b2), (7,b3), (9,b4)]
            rightStep = snd $binaryChildSearchRec "0-0-1" "0-0-" 3 [(78,b6), (123,b7), (43367,b8)]

{-
Testing the helper functions...
-}
test_rule2String = out @=? rule2String "myChain" defaultRule
    where   out = "-A myChain " ++ (box2String defaultBox) ++ " -j DROP\n"
test_rawRule2String = out @=? rawRule2String "myChain" fullNegRawRule
    where   out = "-A myChain " ++ (rawBox2String fullNegRawBox) ++ " -j DROP\n"     
test_rawRule2StringAgain = out @=? rawRule2String "myChain" rule
    where   rule = RawRule (RawBox (False,s) (False,d) (False,(21, 21)) (False,(22, 22)) (False,(17, 17))) (Chain "ACCEPT")
            Right s = subnetParser "192.168.0.2"
            Right d = subnetParser "192.168.2.0"
            out = "-A myChain -m iprange --src-range 192.168.0.2-192.168.0.2 --dst-range 192.168.2.0-192.168.2.0 -p 17 --sport 21:21 --dport 22:22 -j ACCEPT\n"

test_box2String = "-p 0" @=? box2String defaultBox
test_box2StringPartlyRemoveIPWildcard = 
    "-m iprange --dst-range 0.0.0.0-0.0.0.0 -p 0" @=? box2String (Box univS (0,0) (0,0) (0,0) (0,0))

test_rawBox2StringSimple = out @=? rawBox2String rawBox
    where   rawBox = RawBox (False,s) (False,d) (False,(21, 21)) (False,(22, 22)) (False,(17, 17))
            Right s = subnetParser "192.168.0.2"
            Right d = subnetParser "192.168.2.0"
            out = "-m iprange --src-range 192.168.0.2-192.168.0.2 --dst-range 192.168.2.0-192.168.2.0 -p 17 --sport 21:21 --dport 22:22"
test_rawBox2String = out @=? rawBox2String fullNegRawBox
    where   out = "-m iprange ! --src-range 0.0.0.21-0.0.0.21 ! --dst-range 0.0.0.21-0.0.0.21 ! -p 17 ! --sport 21:21 ! --dport 21:21"
test_rawBox2StringProtNeg = out @=? rawBox2String (RawBox (False, univS) (False, univD) (False, (1,1)) (False, (1,1)) (True, (21,21)))
    where   out = "! -p 21"
test_rawBox2StringIPsrcNeg = out @=? rawBox2String (RawBox (True, (45,77)) (False, univD) (False, (1,1)) (False, (1,1)) (True, (21,21)))
    where   out = "-m iprange ! --src-range 0.0.0.45-0.0.0.77 ! -p 21"