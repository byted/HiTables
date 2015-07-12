module IPtablesTransform.Selector where


import Data.List
import Debug.Trace

import IPtablesTransform.Datatypes
import IPtablesTransform.Rule


blocksizeThreshold = 10 -- how many rules are needed to start an HiCuts optimization of a subset of rules


{-
Takes a RawChain and returns a list of Blocks suitable for HiCuts algorithm
-}
getRelevantBlocks :: RawChain -> [Block]
getRelevantBlocks (_, rules) = transformToBlock suitableGroups
    where   
        suitableGroups = filter (\xs -> length xs >= blocksizeThreshold) . groupBy suitability . zip [0..] $ rules  -- find suitable blocks and check if big enough                               
        suitability x y = (isSuitable $ snd x) && (isSuitable $ snd y)                          -- not suitable rules are not grouped together ([F,F,F]→[[F],[F],[F]])!
        isSuitable rule = (isUDP rule || isTCP rule) && (not $ isProtocolNegated rule)          -- e.g: groupBy (\x y-> x&&y) [True, True, False, False, False]
        isSuitableGroup group = (fst . snd . head $ group)


{-
Transform to Block [(0,r0),…,(9,r9)] → ((0,9), [r0,…,r9])
-}
transformToBlock :: [[(Int, RawRule)]] -> [Block] 
transformToBlock input = zipWith (\x (y,z) -> (x, y, z)) [0..] $ map trans input
    where   trans group = (traceBlocks (fst $ head group, fst $ last group), snd $ unzip group)


traceBlocks :: Offset -> Offset
traceBlocks offset = trace ("Rules " ++ show (fst offset) ++ " to " ++ show (snd offset) ++ " are suitable for HiCuts\n") offset