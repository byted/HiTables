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

main = do
    (cntRules:(typ:_)) <- getArgs     
    let cntRulesNum = read cntRules :: Word32
        typeNum = read typ :: Int
        in  if typeNum == 1 then
                print . size . sortRulesByIsDistinct $ createRules cntRulesNum
            else
                print . size . 