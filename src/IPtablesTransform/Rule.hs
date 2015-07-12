{- Author: Stefan Selent -}
module IPtablesTransform.Rule where

import Data.Word
import Data.Bits
import Data.List as L

import IPtablesTransform.Datatypes



univS = (0, 2^32-1)
univD = univS
univSP = (0, 2^16-1)
univDP = univSP
univP = (0, 2^8-1)




{- shortcuts for getting a rules and boxes -}
defaultRawBox = RawBox  (False, univS)
                        (False, univD)
                        (False, univSP)
                        (False, univDP)
                        (False, univP)

fullNegRawBox = RawBox  (True, (21, 21))
                        (True, (21, 21))
                        (True, (21, 21))
                        (True, (21, 21))
                        (True, (17, 17))

defaultRawRule = RawRule defaultRawBox (Chain "DROP")
fullNegRawRule = RawRule fullNegRawBox (Chain "DROP")

defaultBox = Box    univS
                    univD
                    univSP
                    univDP
                    univP

udpBox = Box    univS
                univD
                univSP
                univDP
                (17,17)

tcpBox = Box    univS
                univD
                univSP
                univDP
                (6,6)

defaultRule = Rule defaultBox (Chain "DROP")

dimNames = [IPSrc, IPDst, PortSrc, PortDst, Protocol]

{- Operations on (raw)rules, (raw)boxes and (raw)intervals -}
isProtocol :: Word32 -> RawRule -> Bool
isProtocol num (RawRule box _) = fst protIntervall == num && snd protIntervall == num
    where   protIntervall = snd $ raw_prot box

isUDP :: RawRule -> Bool
isUDP rawRule = isProtocol 17 rawRule

isTCP :: RawRule -> Bool
isTCP rawRule = isProtocol 6 rawRule

isProtocolNegated :: RawRule -> Bool
isProtocolNegated (RawRule box _) = fst $ raw_prot box  

{-
Returns the numerical width of the given interval
-}
rangeFromInterval :: Interval -> Int
rangeFromInterval (lb, ub) 
    | lb <= ub = (((fromIntegral ub :: Int) - (fromIntegral lb :: Int)) + 1)   -- intervals are inclusive!
    | otherwise = 0

interval2Protocol :: Interval -> String
interval2Protocol (a, _) = show a

rawInterval2Protocol :: RawInterval -> String
rawInterval2Protocol (_, interv) = interval2Protocol interv

interval2IPrange :: Interval -> String
interval2IPrange (a, b) = word32ToIP a ++ "-" ++ word32ToIP b

interval2Range :: Interval -> String
interval2Range (a, b) = show a ++ ":" ++ show b

word32ToIP :: Word32 -> String
word32ToIP a =      (show $ (a `shiftR` 24) .&. 255)  ++ "."
                ++  (show $ (a `shiftR` 16) .&. 255) ++ "."
                ++  (show $ (a `shiftR` 8) .&. 255) ++ "."
                ++  (show $ a .&. 255)

{-
Checks whether a given RawRule contains any negated dimension
-}
hasNegation :: RawRule -> Bool
hasNegation (RawRule box _) 
    = foldl (\acc (neg, iv) -> neg  ) False (rawBoxAsList box)

{-
Converts a given RawRule to simple Rule by cuting off the negation part
-}
toRule :: RawRule -> Rule
toRule (RawRule rawBox action) = Rule box action
    where   box = Box s d sp dp p
            [s,d,sp,dp,p] = map (\(neg, iv) -> iv) (rawBoxAsList rawBox)

{-
Take a rawBox and convert it in list of rawIntervals of length 5
-}
rawBoxAsList :: RawBox -> [RawInterval]
rawBoxAsList (RawBox s d sp dp p) = [s,d,sp,dp,p]

{-
Like rawBoxAsList, only for simple boxes
-}
boxAsList :: Box -> [Interval]
boxAsList (Box s d sp dp p) = [s,d,sp,dp,p]

listToBox :: [Interval] -> Box
listToBox (s:d:sp:dp:p:_) = Box s d sp dp p

{-
gets the n-th dimension of a given box
-}
getIntervalByIndex :: Int -> Box -> Interval
getIntervalByIndex d b = 
    case d of
        0   -> src_addrs b
        1   -> dst_addrs b
        2   -> src_ports b
        3   -> dst_ports b
        4   -> prot b 

{-
Check if two given boxes touch each other in a given dimension
-}
boxesTouching :: Box -> Box -> Int -> Bool
boxesTouching b1 b2 d = (ub2 >= lb1 && ub1 >= lb2)
        where   (lb1, ub1) = getIntervalByIndex d b1
                (lb2, ub2) = getIntervalByIndex d b2

{-
Merges 2 boxes under the assumption that the boxes are touching or next to each other
and the first box is "lower" than the 2nd one
-}
mergeBoxes :: Box -> Box -> Box
mergeBoxes (Box s1 d1 sp1 dp1 p1) (Box s2 d2 sp2 dp2 p2) =
    Box (fst s1, snd s2) (fst d1, snd d2) (fst sp1, snd sp2) (fst dp1, snd dp2) (fst p1, snd p2)

mergeBoxList :: [Box] -> Box
mergeBoxList (b:bs) = foldl mergeBoxes b bs

{-
Takes a RawRule and returns a list of normalized Rules by
replacing nagations
-}
ruleNormalizer :: RawRule -> [Rule]
ruleNormalizer (RawRule rawBox a) = normalize ds                            
    where   ds = zip universes (rawBoxAsList rawBox)                            -- we get pairs in the form of (universe, rawIV)
                where universes = boxAsList defaultBox
            normalize xs = 
                map boxToRule . perm $ map getIntervals xs
            getIntervals (_, (False, iv)) = [iv]                                -- d no negated → return interval
            getIntervals (uv, (True, iv)) = calcSubIntervals iv uv              -- d negated → return 2 sub intervals
            calcSubIntervals (negLb, negUb) (univLb, univUb)
                | negLb <= univLb && negUb < univUb = [(negUb + 1, univUb)]     -- don't wanna cause an overflow 
                | negLb > univLb && negUb >= univUb = [(negLb - 1, univLb)]     -- so check the border cases
                | negLb <= univLb && negUb >= univUb = []                       -- we have an IV which will never match!
                | otherwise = [(univLb, negLb - 1),(negUb + 1, univUb)]  
            perm [s, d, sp, dp, p] =                                            -- get all filter combinations as list of 5 tupels
                [(x1, x2, x3, x4, x5) | x1 <- s, x2 <- d, x3 <- sp, x4 <- dp, x5 <- p]
            boxToRule (s,d,sp,dp,p) =                                           -- take a box, append the action of the original 
                Rule (Box s d sp dp p) a

isIPWildcard :: Interval -> Bool
isIPWildcard (a, b) = a == (fst univS) && b == (snd univS)

getBoxes :: [Rule] -> [Box]
getBoxes rx = L.map (\(Rule box _) -> box) rx

{--
Some basic rule optimizations
--}

subset :: Rule -> Rule -> Bool
subset (Rule box1 _) (Rule box2 _) = 
	isIncluded (src_addrs box1) (src_addrs box2) &&
	isIncluded (dst_addrs box1) (dst_addrs box2) &&
	isIncluded (src_ports box1) (src_ports box2) &&
	isIncluded (dst_ports box1) (dst_ports box2) &&
	isIncluded (prot box1) (prot box2) 
	where	isIncluded (lb1, ub1) (lb2, ub2) = lb1 >= lb2 && ub1 <= ub2

sameAction :: Rule -> Rule -> Bool
sameAction (Rule box1 a1) (Rule box2 a2) = a1 == a2

removeShadowedRules :: [Rule] -> [Rule]
removeShadowedRules [] = []
removeShadowedRules (rule:rules) = rule:(removeShadowedRules $ removeShadowing rule rules [])

removeShadowing :: Rule -> [Rule] -> [Rule] -> [Rule]
removeShadowing _ [] acc = reverse acc
removeShadowing rule (r:rs) acc = if (subset r rule) then (removeShadowing rule rs acc) else (removeShadowing rule rs $ r:acc)


{- redundancy removal -}

removeRedundantRules :: [Rule] -> [Rule]
removeRedundantRules [] = []
removeRedundantRules (r:rs) = removeRedundancy r rs ++ removeRedundantRules rs

removeRedundancy :: Rule -> [Rule] -> [Rule]
removeRedundancy r [] = [r]
removeRedundancy rule (r:rs) = if (subset rule r) && (sameAction rule r) then [] else removeRedundancy rule rs