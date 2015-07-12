{- Author: Stefan Selent -}
module IPtablesTransform.Emitter
where

import Data.List
import Debug.Trace

import IPtablesTransform.Datatypes
import IPtablesTransform.Tree
import IPtablesTransform.Rule
import IPtablesTransform.Utils

tree2List ::
    String ->   -- chain base name
    String ->   -- identifies the block
    ChildSearchType ->
    Tree ->     -- 
    [String]    -- list of create chains cmds followed by rule commands 
tree2List base block sType root = createChainsAsStrings ++ ["",""] ++ rulesAsStrings ++ defaultPoliciesAsStrings
    where   rulesAsStrings = map r2sWrapper (jumpsWithBase ++ rules)
                where   r2sWrapper (chain, rule) =  if chain == "0" then 
                                                        rule2String base rule 
                                                    else 
                                                        rule2String (base ++ block ++ chain) rule
            (lastChainNum, jumps, rules) = chainIt root 0 0 sType 
            jumpsWithBase = map (\(c, Rule b (Chain a)) -> (c, Rule b . Chain $ base ++ block ++ a)) jumps
            createChainsAsStrings = map toChainCmd jumpsWithBase
            defaultPoliciesAsStrings = map toDefPolicyCmd jumpsWithBase
            toChainCmd (_, Rule _ (Chain dstC)) = "-N " ++ dstC ++ "\n"
            toDefPolicyCmd (_, Rule _ (Chain dstC)) = "-A "++ dstC ++ " -j ACCEPT\n"

{-
Takes a tree and a chain id offset and returns a triple of the last chain id used,
a list of generated jump rules and a list of original rules.
This function is responsible for generating a chains structure from a tree
-}
chainIt :: 
    Tree ->
    Int ->  -- depth 
    Int ->  -- Offset
    ChildSearchType ->
    (Int, [JumpRule], [OriginalRule])                                       -- (last chain id, jumpRules, originalRules (chain to append to, the rule to append))
chainIt (Node _ rx []) _ offset sType = (offset, [], zip (repeat $ show offset) rx)        -- if there are no children, no need to generate jchain jump rules
chainIt (Node _ _ cx) depth offs sType = (if nextOffset == 0 then 0 else nextOffset-1, newJumpRules ++ jumpRules, origRules)
    where   ((nextOffset, chainIds), (jumpRules, origRules)) = chainChildren (depth + 1) offs cx sType
            newJumpRules = childSearch sType depth offs chainIds (tail cx) -- remove the current chain

{-
This function collects the information from the children. This is a depth first approach to propagate the current chainId.
Return: Pair of:    Pair of nextChainId and List of chainIds for each child
                    Pair of crecursively created rules sorted by type (Jump- or Original)
-}
chainChildren :: 
    Int ->      -- depth
    Int ->      -- initial chainId (the id of the parent node)
    [Tree] ->   -- list of children to examine
    ChildSearchType ->
    ((Int, [Int]), ([JumpRule], [OriginalRule]))
chainChildren depth offs cx sType = ((nextId, tail $ reverse chainIds), (concat jmps, concat origs))    -- tail removes the initialChainId
    where   (jmps, origs) = unzip rules
            (nextId:chainIds, rules) = mapAccumL collect [offs] cx
            collect ids node = ((recId + 1):ids, (recJumpRules, recOrigRules))     
                where   (recId, recJumpRules, recOrigRules) = chainIt node depth (head ids) sType

{-
Wrapper function for building the child selection structure. We can't use array-like structures in iptables. Therefore
different approaches are needed.
-}
childSearch::
    ChildSearchType ->  -- type of search: 'Linear' or 'Binary'
    Int ->              -- depth
    Int ->              -- initial chainId (parent Id - the src of the jump) 
    [Int] ->            -- list of chainIds
    [Tree] ->           -- list of children to search through
    [JumpRule]          -- list of jumps describing search algo
childSearch sType depth srcId ids nodes = if sType == Linear then linearChildSearch (show srcId) pairs else binaryChildSearch (show srcId) (show depth) pairs
    where   pairs = zip ids $ map filterBox nodes

linearChildSearch :: String -> [(Int, Box)] -> [JumpRule]  
linearChildSearch srcId cs = 
    foldr (\(idx, box) acc -> (srcId, Rule box . Chain $ show idx):acc) [] cs

binaryChildSearch :: 
    String ->           -- parent id
    String ->           -- depth
    [(Int, Box)] ->
    [JumpRule]
binaryChildSearch parentId depth cs
    | length cs < 3 = linearChildSearch parentId cs
    | otherwise = snd $ binaryChildSearchRec parentId (parentId ++ "-" ++ depth ++ "-") 0 cs

binaryChildSearchRec :: 
    String ->           -- parent id
    String ->           -- base id for this tree xxx-depth
    Int ->              -- id for
    [(Int, Box)] ->
    (Int, [JumpRule])
binaryChildSearchRec parentId baseId binChainId cs
    | length cs == 1 = (binChainId, []) 
    | length cs < 3 = (binChainId, linearChildSearch parentId cs)
    | otherwise = (rightNextChainId, mRule:lRule:rRule:(leftSearch ++ rightSearch))
    where   (left, [m], right) = splitIn3 cs
            mergedLeftBoxes = mergeBoxList . snd $ unzip left
            mergedRightBoxes = mergeBoxList . snd $ unzip right
            lId = if length left == 1 then show . fst $ head left  else baseId  ++ (show binChainId)
            rId = if length right == 1 then show . fst $ head right else baseId ++ (show $ binChainId + 1)
            mRule = (parentId, Rule (snd m) . Chain $ (show $ fst m))
            lRule = (parentId, Rule mergedLeftBoxes $ Chain lId)        
            rRule = (parentId, Rule mergedRightBoxes $ Chain rId)
            (leftNextChainId, leftSearch) = binaryChildSearchRec lId baseId (binChainId + 2) left
            (rightNextChainId, rightSearch) = binaryChildSearchRec rId baseId leftNextChainId right



{-
Helper functions
-}

rule2String :: 
    String ->   -- name of the chain to which we append the rule
    Rule ->     -- the rule
    String
rule2String chain (Rule box (Chain action)) = 
    intercalate " " ["-A", chain, (box2String box), "-j", action] ++ "\n" 

rawRule2String :: 
    String ->   -- name of the chain to which we append the rule
    RawRule ->     -- the rule
    String
rawRule2String chain (RawRule rawBox (Chain action)) = 
    intercalate " " ["-A", chain, (rawBox2String rawBox), "-j", action] ++ "\n" 

rawBox2String :: RawBox -> String
rawBox2String b 
    | isNegated (raw_src_addrs b) == "!" && isIPWildcard (snd $ raw_src_addrs b) = ""
    | isNegated (raw_dst_addrs b) == "!" && isIPWildcard (snd $ raw_dst_addrs b) = ""
    | otherwise = ipPrefix ++ intercalate " " ([
                                    isNegated (raw_prot b) ++ "-p"
                                ,   interval2Protocol $ snd (raw_prot b)
                                ] ++ if rawInterval2Protocol (raw_prot b) == "6" || rawInterval2Protocol (raw_prot b) == "17" then [
                                        isNegated (raw_src_ports b) ++ "--sport"
                                    ,   interval2Range $ snd (raw_src_ports b)
                                    ,   isNegated (raw_dst_ports b) ++ "--dport"  
                                    ,   interval2Range $ snd (raw_dst_ports b)
                                ] else [])

    where   isNegated x = if fst x then "! " else ""
            ipPrefix = case (isIPWildcard . snd . raw_src_addrs $ b, isIPWildcard . snd . raw_dst_addrs $ b) of
                        (True, True) -> ""
                        (True, False) -> "-m iprange " ++ isNegated (raw_dst_addrs b) ++ "--dst-range " ++ interval2IPrange (snd $ raw_dst_addrs b) ++ " "
                        (False, True) -> "-m iprange " ++ isNegated (raw_src_addrs b) ++ "--src-range " ++ interval2IPrange (snd $ raw_src_addrs b) ++ " "
                        (False, False) ->   "-m iprange " ++ isNegated (raw_src_addrs b) ++ "--src-range " ++ interval2IPrange (snd $ raw_src_addrs b) ++ " "
                                            ++ isNegated (raw_dst_addrs b) ++  "--dst-range " ++ interval2IPrange (snd $ raw_dst_addrs b) ++ " "

box2String :: Box -> String
box2String b =  ipPrefix ++ intercalate " " ([
                                    "-p"
                                ,   interval2Protocol $ prot b
                                ] ++ if interval2Protocol (prot b) == "6" || interval2Protocol (prot b) == "17" then [
                                    "--sport"
                                ,   interval2Range $ src_ports b
                                ,   "--dport"
                                ,   interval2Range $ dst_ports b
                                ] else [])
    where   ipPrefix =  case (isIPWildcard . src_addrs $ b, isIPWildcard . dst_addrs $ b) of
                        (True, True) -> ""
                        (True, False) -> "-m iprange --dst-range " ++ (interval2IPrange . dst_addrs $ b) ++ " "
                        (False, True) -> "-m iprange --src-range " ++ (interval2IPrange . src_addrs $ b) ++ " "
                        (False, False) ->   "-m iprange --src-range " ++ (interval2IPrange . src_addrs $ b)
                                            ++ " --dst-range " ++ (interval2IPrange . dst_addrs $ b) ++ " "               