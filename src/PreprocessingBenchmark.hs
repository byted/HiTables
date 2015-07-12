import System.Environment
import Data.Word
import Data.HashSet as H
import Data.List as L

import IPtablesTransform.Rule
import IPtablesTransform.TransformationEngine


createRules x = L.map (\i -> Rule (Box univS univD univSP (i,i) (17,17)) (Chain "DROP")) [0..(x - 1)]


sortRulesByIsDistinct :: [Rule] -> HashSet (String, Int)
sortRulesByIsDistinct rx = foldl sortRule H.empty [(x, y) | x <- bx, y <- dimNames]
    where   bx = getBox rx
            sortRule isDistinct (b, dim) = 
                if isDistinctInDim b (L.delete b bx) dim then H.insert (show b, fromEnum dim) isDistinct else isDistinct


isDistinct :: Rule -> [Rule] -> [Bool]
isDistinct (Rule box _) rx = L.map (isDistinctInDim box bx) dims
    where bx = L.map (\(Rule b _) -> b) rx

cntDistinctBoxesDP :: [Rule] -> HashSet (String, Int) -> Int
cntDistinctBoxesDP rx isDistinct = foldl (\acc b -> if H.member (show b, 3) isDistinct then acc + 1 else acc) 0 $ getBox rx



main = do
    (cntRules:(rounds:_)) <- getArgs    
    let roundsNum = read rounds :: Int
        cntRulesNum = read cntRules :: Word32
        isDistinct = sortRulesByIsDistinct $ createRules cntRulesNum
        count acc i = acc + (cntDistinctBoxesDP (createRules cntRulesNum) isDistinct)
        in print $ foldl count 0 [0..(roundsNum - 1)]