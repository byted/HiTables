import System.Environment
import Data.Word

import IPtablesTransform.Rule
import IPtablesTransform.TransformationEngine

createRules x = map (\i -> Rule (Box univS univD univSP (i,i) (17,17)) (Chain "DROP")) [0..(x - 1)]
getBox rx = map (\(Rule box _) -> box) rx

cntDistRls :: [Rule] -> [Int]
cntDistRls rx = map (countDistinctBoxes $ getBox rx) [IPSrc, IPDst, PortSrc, PortDst, Protocol]  

cntDistinctBoxesDP :: [Rule] -> Int
cntDistinctBoxesDP rx = countDistinctBoxes (getBox rx) PortDst


main = do
    (cntRules:(rounds:_)) <- getArgs    
    let roundsNum = read rounds :: Int
        cntRulesNum = read cntRules :: Word32
        count acc i = acc + (cntDistinctBoxesDP $ createRules cntRulesNum)
        in print $ foldl count 0 [0..(roundsNum - 1)]