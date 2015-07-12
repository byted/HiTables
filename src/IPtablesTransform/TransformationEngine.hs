{- Author: Stefan Selent -}
module IPtablesTransform.TransformationEngine
where

import Data.List as L
import Data.Ord
import Data.Word
import Data.HashSet as H
import Debug.Trace

import IPtablesTransform.Datatypes
import IPtablesTransform.Utils
import IPtablesTransform.Tree
import IPtablesTransform.Rule
import IPtablesTransform.Emitter (tree2List, rawRule2String)
import IPtablesTransform.Selector (getRelevantBlocks)


getDimAlgo :: DimensionAlgorithmName -> DimensionAlgorithm
getDimAlgo MinMaxRulesPerChild = minMaxRulesPerChild
getDimAlgo MinSM = minSmDim
getDimAlgo MaxDistinctRules = maxDistinctRuleComponents


rulesX =    [   RawRule (RawBox (False,(50,50)) (False,univD) (False,univSP) (False,univDP) (False,(6,6))) (Chain "ACCEPT")
            ,   RawRule (RawBox (False,(50,50)) (False,univD) (False,univSP) (False,univDP) (False,(6,6))) (Chain "ACCEPT")
            ,   RawRule (RawBox (False,(50,50)) (False,univD) (False,univSP) (False,univDP) (False,(6,6))) (Chain "ACCEPT")
            ,   RawRule (RawBox (False,(50,50)) (False,univD) (False,univSP) (False,univDP) (False,(6,6))) (Chain "ACCEPT")
            ]

nodeX = Node defaultBox (concatMap ruleNormalizer rulesX) []

convert :: 
    [RawChain] -> 
    Int ->                  -- binth
    Int ->                  -- spfac
    DimensionAlgorithmName ->
    ChildSearchType -> 
    (String, [ChainStats])
convert xs binth spfac dimAlgo sType = foldl accChains ("", []) xs
    where   accChains (accStr, accStats) (chain, rrx) = 
                let (ruleStr, stats) = convertChain (chain, rrx) binth spfac dimAlgo sType
                    in (accStr ++ ruleStr, accStats ++ [stats])

{-
Takes a rawChain and options and returns a list of rules (as strings). 
Suitable ranges of rules are found, converted and put back into the original list of rules
-}
convertChain :: 
    RawChain ->
    Int ->                  -- binth
    Int ->                  -- spfac
    DimensionAlgorithmName ->
    ChildSearchType ->
    (String, ChainStats)
convertChain (chain, rrx) binth spfac dimAlgo sType = 
    (concat . frth $ foldl mergeTreesIntoRules (chain, sType, 0, rules) trees, (chain, L.map getStats . trd $ unzip3 trees))
    where   blocks = getRelevantBlocks (chain, rrx)                                             -- take only ranges of rules suitable for algorithm
            rules = L.map (rawRule2String chain) rrx                                            -- converts parsed rules back to strings
            trees = L.map (\(index, (low, high), rr) -> (index, (low, high), buildHiCutsTree rr binth spfac dimAlgo)) blocks

{-
Replace a block by it's tree and preserve order by calculating a new block-offset.
(Replacing rules with a tree will result in more rules thus the position of the other blocks has to be adapted)
-}
mergeTreesIntoRules :: 
    (String, ChildSearchType, Int, [String]) ->   -- (chain name, search type, new block-offset caused by inserting previous tree, list of rules including all prev merged trees)
    (Int, (Int, Int), Tree) ->  -- (index of the block, (beginning of block in original ruleset, end of block in original ruleset), tree to merge)
    (String, ChildSearchType, Int, [String])
mergeTreesIntoRules (chain, sType, blockOffset, ruleList) (blockIndex, (low, high), tree) =
    (chain, sType, newOffset + blockOffset, insertAt ruleList (low + blockOffset) (high + blockOffset) $ treeAsList)
        where   treeAsList = tree2List chain ("-B" ++ (show blockIndex) ++ "-") sType tree
                newOffset = length treeAsList - ((high - low) + 1)      -- we expand the list of rules with our tree so we need the number of new rules to later calculate the new block limits

-- in purgedTree `debug` (" after: " ++ (let (n,_,_,_) = getStats purgedTree in show n))
{-
Wrapper function. Takes a list of rules and the parameters as described. In the end,
there is a tree, HiCuts style. Optimizations included
-}
buildHiCutsTree :: 
    [RawRule] ->            -- RawRules to construct tree from
    Int ->                  -- binth (max rules per leaf)
    Int ->                  -- spfac (space measure function parameter)
    DimensionAlgorithmName ->   -- algorithm to pick the dimension
    Tree                    -- constructed tree
buildHiCutsTree [] _ _ _ = EmptyTree
buildHiCutsTree rawRules binth spfac dimAlgoName =
    let spmf n = spfac * n
        rules = removeRedundantRules . removeShadowedRules $ concatMap ruleNormalizer rawRules
        distRules = sortRulesByIsDistinct (rules {-`debug` (show rules)-})                         -- create a HashSet of distinct rules
        root = Node initialBox rules []                                 -- initialize the root node
        initialBox = if isUDP $ head rawRules then udpBox else tcpBox   -- we implicitly assume that there is only UDP and TCP!
        tree = transformNode binth spmf (getDimAlgo dimAlgoName) (distRules {-`debug` (show $ toList distRules)-}) 0 root   -- start recursive transformation
        purgedTree = purgeTree tree
        in purgedTree `debug` (" after: " ++ (let (n,_,_,_) = getStats purgedTree in show n))

{-
Checks if a node is suitable for cuting and recursively checks the children of this node
-}
transformNode ::
    Int ->                  -- binth
    Spmf ->                 -- spmf
    DimensionAlgorithm ->   -- dimAlgo
    DistinctRules ->        -- HashSet of distinct rules
    Int ->                  -- repetition counter
    Tree ->
    Tree
transformNode binth spmf dimAlgo distRules counter node
    | counter > 1 = node
    | (length $ rules node) <= binth = node
    | otherwise = Node newBox newRules newChildren
        where   (Node newBox newRules cx) = (dimAlgo node spmf distRules) 
                newCounter = if any (== (length $ rules node)) $ L.map (\c -> length $ rules c) cx
                            then counter + 1
                            else 0
                newChildren = L.map (transformNode binth spmf dimAlgo distRules newCounter) cx 



{-
Takes a node, a dimension and the number of cuts and creates resulting
child nodes and appends them to the original node and returns it.
Additionally, the parent's rules are moved to the appropriate children.
-}
cut :: 
    Tree ->             -- node to be cut
    Int ->              -- dimension to cut 
    Int ->              -- how many cuts
    Tree                -- original node with newly created nodes appended
cut EmptyTree _ _        = EmptyTree
cut (Node box rx _) dim cuts = (Node box [] childNodes) --`debug` ("cut dim " ++ show dim ++ " " ++ show cuts ++ " times")
    where   (lb, ub) = getIntervalByIndex dim box
            childNodes = categorizeRules rx dim $ unfoldr createNode dimsAfterCut
            dimsAfterCut = newIntervals (lb, ub) cuts
            createNode [] = Nothing
            createNode (x:xs) = 
                Just (Node (modifyBoxDim box x dim) [] [], xs)

{-
    -- Number of partitions / cuts algorithms
-}

{-
Returns the best suited cut for a given node and dimension in terms of
space measure fuction described in HiCuts
-}
getBestCut ::
    Tree ->             -- node to be cut
    Int ->              -- dimension to cut
    Spmf ->             -- spmf
    Tree                -- cutted node
getBestCut node dim spmf = checkCuts initValue
    where
        initValue = maximum $ [4, intSqrt numOfRules]
        numOfRules = length $ rules node 
        intervalRange = rangeFromInterval . getIntervalByIndex dim . filterBox $ node
        checkCuts numP =    -- stop increasing the num of cuts if bigger than spmf or if next num of cut would be bigger than the interval itself
            if sm testCut >= spmf numOfRules || nextNumP >= intervalRange  then    
                testCut
            else checkCuts nextNumP
            where   testCut = cut node dim numP
                    nextNumP = numP * 2



{-
    -- Dimension picking algorithms
-}

{-
HiCuts a)
Chose dimension to cut by minimizing the maximum number of rules in a child node
-}
minMaxRulesPerChild :: 
    Tree ->         -- node to be cut
    Spmf ->         -- spfm
    DistinctRules ->         
    Tree            
minMaxRulesPerChild node spmf _ = bestCut (testCuts )--`debug` ("possible dims: " ++ (show $ length testCuts)))
        where   bestCut [] = node --`debug` ("num of children: " ++ (show . length $ children node))                  -- don't cut, all dimensions are too small
                bestCut cuts = minimumBy (comparing maxRulesPerChild) cuts
                testCuts = getTestCuts node spmf
                maxRulesPerChild n = bla --`debug` ("maxRulesPerChild: " ++ (show bla)) 
                    where   bla = maximum . L.map (length . rules) $ children n

{-
HiCuts c)
Minimize sm over all dimensions
-}
minSmDim ::
    Tree ->         -- node to be cut
    Spmf ->         -- spfm
    DistinctRules ->
    Tree
minSmDim node spmf _ = bestCut $ getTestCuts node spmf
    where   bestCut [] = node
            bestCut cuts = minimumBy (comparing sm) cuts


{-
HiCuts d)
distinct rule components := component which does not touch _any_ other component
-}
maxDistinctRuleComponents ::
    Tree ->
    Spmf -> -- spfm
    DistinctRules ->
    Tree
maxDistinctRuleComponents node spmf distRules = 
    if noDecision {-`debug` ("no decision: rules " ++ (show . length $ rules node))-} then
        minMaxRulesPerChild node spmf distRules
    else
        getBestCut node (bestDim {-`debug` ("** winner is: " ++ show bestDim)-}) spmf  -- get index of dim with highest count of distinct rules  
    where   distinctBoxesPerDim = L.map (countDistinctBoxes (boxesOfRules node) distRules) [0..4]
            noDecision = all (== head distinctBoxesPerDim) (tail distinctBoxesPerDim)
            bestDim = fst . maximumBy (\(_,x) (_,y) -> compare x y) . reverse . zip [0..] $ (distinctBoxesPerDim {-`debug` ("** distinct boxes: " ++ show distinctBoxesPerDim)-})

countDistinctBoxes :: [Box] -> HashSet (String, Int) -> Int -> Int
countDistinctBoxes bx distRules dim = 
    foldl (\acc b -> if H.member (show b, fromEnum dim) distRules then acc + 1 else acc) 0 $ bx



{-
Returns a list of sub-intervals, derived from a universe and the number of cuts
-}
newIntervals :: Interval -> Int -> [Interval]
newIntervals (lb, ub) numOfCuts
    | numOfCuts < 1 = []
    | otherwise = 
        reverse $ unfoldr createIntervals numOfChilds 
        where
            newRange = rangeFromInterval (lb, ub) `div` numOfChilds
            numOfChilds = numOfCuts + 1
            createIntervals x = if x > 0 then Just (intervalFor x, x-1) else Nothing
            intervalFor x
                | x > 1 = let newub = (asInt ub) - (numOfChilds - x) * newRange in (asWord(newub - newRange + 1), asWord(newub))
                | otherwise = (lb, ub - asWord((numOfChilds - x) * newRange))

{- 
Takes a box, an interval and the dimension and replaces the
specified dimension interval with the given one
-}
modifyBoxDim :: Box -> Interval -> Int -> Box              
modifyBoxDim box modInterval d = 
    listToBox . replaceNth d modInterval $ boxAsList box

{-
Takes a list of rules and appends them to their appropriate
nodes of the given list by matching the dimension
-}
categorizeRules ::
    [Rule] ->           -- list of rules to be categorized
    Int ->              -- dimension to categorize on
    [Tree] ->           -- list of nodes which serve es the categories
    [Tree]              -- list of nodes with categorized rules appended
categorizeRules rs d ns = zipWith appendRules ns matchingRulesList          -- for each node check every rule if it touches
        where   matchingRulesList = L.map forEachNode ns
                forEachNode node = foldl (createListOfTouchingRulesFor node) [] rs
                createListOfTouchingRulesFor node acc rule = 
                    if boxesTouching nodeBox ruleBox d then rule:acc else acc
                        where   Rule ruleBox _ = rule
                                Node nodeBox _ _ = node
{-
Takes a list of rules and returns a hash set of distinct ones
-}
sortRulesByIsDistinct :: [Rule] -> HashSet (String, Int)
sortRulesByIsDistinct rx = foldl sortRule H.empty [(x, y) | x <- bx, y <- [0..4]]
    where   bx = getBoxes rx
            sortRule isDistinct (b, dim) = 
                if isD {-`debug` (show isD)-} then H.insert (show b, fromEnum dim) isDistinct else isDistinct
                where  isD = isDistinctInDim b (L.delete b bx) dim

isDistinctInDim :: Box -> [Box] -> Int -> Bool
isDistinctInDim box bx dim = 
    0 == (length $ L.filter ((flip $ boxesTouching box) dim) bx)


{-
Cuts a node in all dimensions and returns the cuts as a list
-}
getTestCuts :: Tree -> Spmf -> [Tree]
getTestCuts node spmf = L.map cutDim $ L.filter dimBigEnough [0..4]
    where   dimBigEnough d = 4 < (rangeFromInterval $ getIntervalByIndex d (filterBox node))    -- filter out dimensions where the intervall is too small 
            cutDim dim = getBestCut node dim spmf                                               -- better: sqrt(length $ rules node) < ?