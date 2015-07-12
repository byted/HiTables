{- Author: Stefan Selent -}
module IPtablesTransform.Tree
where

import Debug.Trace

import IPtablesTransform.Datatypes
import IPtablesTransform.Rule (defaultBox)
import IPtablesTransform.Utils

instance Show Tree where
      show EmptyTree = "EmptyTree"
      show (Node fil rs cs) = 
        show fil ++ "\nRules:\n" ++ pList' rs ++ "\nChildren\n" ++ pList' cs 

{-
List in list printer.
-}
pList l = "[\n" ++ concatMap (\x -> pList' x ++ "\n") l ++ "]"
    where   pList' y ="\t[\n" ++ concatMap (\y -> "\t\t" ++ show y ++ "\n") y ++ "\t]"

pListTab' y ="\t[\n" ++ concatMap (\y -> "\t\t" ++ show y ++ "\n") y ++ "\t]"
pList' y ="[\n" ++ concatMap (\y -> "\t" ++ show y ++ "\n") y ++ "]"


{-shortcut for getting a node-}
defaultNode = Node defaultBox [] []

{- shortcut for creating a node -}
singleton :: Box -> [Rule] -> Tree
singleton f rules = Node f rules []

{-
Takes a node and checks if there are any children connected
-}
hasChildren :: Tree -> Bool
hasChildren EmptyTree = False
hasChildren node = if (length . children $ node) > 0 then True else False

{-
Takes a node and a list of rules and appends the rules to this node
-}
appendRules :: Tree -> [Rule] -> Tree
appendRules EmptyTree _ = EmptyTree
appendRules node [] = node
appendRules (Node b rs cs) (x:ruleList) = appendRules (Node b (rs++[x]) cs) ruleList 

{-
Takes a node and a list of nodes and appends the list as children
to the first node
-}
appendChildren :: Tree -> [Tree] -> Tree
appendChildren EmptyTree _ = EmptyTree
appendChildren node [] = node
appendChildren (Node b rs cs) (n:ns) = appendChildren (Node b rs (cs++[n])) ns 

replaceChildren :: Tree -> [Tree] -> Tree
replaceChildren EmptyTree _ = EmptyTree
replaceChildren (Node filterBox rules _) children = Node filterBox rules children 

{-
Takes a node and returns a list of boxes of the rules contained in the node
-}
boxesOfRules :: Tree -> [Box]
boxesOfRules node = map (\(Rule box _) -> box) . rules $ node

{-
Returns statistics for a given node: quadruple (num of nodes, num of leafs, max depth, num of rules in all children)
-}
getStats :: Tree -> (Int, Int, Int, Int)
getStats (Node _ rules []) = if length rules == 0 then (1, 1, 1, length rules) `debug` ("node with 0 rules") else (1, 1, 1, length rules)
getStats (Node _ rules children) =  (nodes, leaf, depth + 1, rules)
    where (nodes, leaf, depth, rules) =
            foldl (\acc node -> mergeQuadruples acc $ getStats node) (1, 0, 1, 0) children

{-
Helper function for getStats
-}
mergeQuadruples :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
mergeQuadruples (x1, x2, depthX, x4) (y1, y2, depthY, y4) = (x1+y1, x2+y2, maximum[depthX, depthY], x4+y4)

{-
Takes a HiCuts tree and removes emty nodes (no children & no rules)
-}
purgeTree :: Tree -> Tree
purgeTree (Node box [] cx) = Node box [] purgedChildren     -- check only the children of inner nodes (inner nodes have no rules)
    where   filteredChildren = filter (\(Node _ rx cx2) -> length rx > 0 || length cx2 > 0 ) cx     -- take only children with either rules or children
            purgedChildren = map purgeTree filteredChildren                                         -- recursively check legit children
purgeTree node = node           -- if we have no inner node it has to be e legit leaf so return it

{-
Returns the number of times the given node was cut
-}
np :: Tree -> Int
np (Node _ _ cs)
    | length cs == 0 = 0
    | otherwise = (length cs) - 1

{-
Measures the space needed for a cut, HiCuts style
-}
sm :: Tree -> Int 
sm EmptyTree = 0
sm node = (np node) + rulesInAllChildren node
    where   rulesInAllChildren x =
                foldl (\acc child -> acc + (length $ rules child)) 0 $ children x