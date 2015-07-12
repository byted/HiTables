{- Author: Stefan Selent -}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
    
-- iptables_transform.hs
import Prelude hiding (catch)
import Control.Exception
import System.IO.Error hiding (catch)
import System.Environment
import System.Console.CmdArgs
import System.Directory
import System.Exit
import Data.List

import IPtablesTransform.Datatypes
import IPtablesTransform.Parser (parseIPtables)
import IPtablesTransform.TransformationEngine (convert)


data Options = Options 
    {   inFile :: String
    ,   outFile :: String
    ,   bbinth :: Int
    ,   sspfac :: Int
    ,   dimensionAlgorithm :: DimensionAlgorithmName
    ,   hiCutsThreshold :: Int
    ,   selectionAlgorithm :: ChildSearchType
    } deriving (Data,Typeable,Show)

options = cmdArgsMode $ Options
    {
        inFile = def            &= argPos 0 &= typ "IN-FILE",
        outFile = def           &= argPos 1 &= typ "OUT-FILE",
        bbinth = 4              &= help "Set binth variable",
        sspfac = 2              &= help "Set spfac variable",
        dimensionAlgorithm = MaxDistinctRules   &= help "Set how the dimension to cut is chosen",
        hiCutsThreshold = 10    &= help "Set the minimum number of rules for invoking a transformation",
        selectionAlgorithm = Binary     &= help "Set algorithm for child selection: 'Linear' or 'Binary'"
    }   &= summary "Optimize an iptables/netfilter set of rules with a HiCuts like algorithm"

prefix = "*filter\n:INPUT ACCEPT\n:FORWARD ACCEPT\n:OUTPUT ACCEPT\n"

suffix = "COMMIT\n"


main = do
    flags <- cmdArgsRun options
    content <- readFile $ inFile flags
    case parseIPtables content of
        Left err -> do print err
        Right parsedRules -> do printNumOfParsedRules parsedRules
                                let (rules, stats) = convert parsedRules (bbinth flags) (sspfac flags) (dimensionAlgorithm flags) (selectionAlgorithm flags)
                                let statsStr =  getChainsStats stats
                                writeFile (outFile flags) $ statsStr ++ prefix ++ rules ++ suffix
                                putStrLn ("wrote modified ruleset to " ++ show (outFile flags))



{-
print helper
-}
printNumOfParsedRules :: [(String, [RawRule])] -> IO ()
printNumOfParsedRules rules = putStrLn ("Parsed " ++ show (foldl (\acc (_,x) -> acc + (length x)) 0 rules) ++ " rules in " ++ show (length rules) ++ " chains")

getChainsStats :: [ChainStats] -> String
getChainsStats xs = foldl chainStatsAsStr "" xs
    where   chainStatsAsStr acc (chainName, stats) =
                acc ++ "# Stats for chain " ++ show chainName ++"\n" ++ getBlocksStats stats ++ "\n"

getBlocksStats :: [(Int, Int, Int, Int)] -> String
getBlocksStats xs = foldl blockStatsAsString "" $ zip [1..] xs
    where   blockStatsAsString acc (i,(numNodes, numLeafs, maxDepth, numRules)) =
                acc ++ "#\tStats for block " ++ show i ++"\n" ++
                "#\tNodes created: " ++ show numNodes ++ ", of them " ++ show numLeafs ++ " leafs." ++
                " Max depth: " ++ show maxDepth ++ ",  # of rules in all children: " ++ show numRules ++ "\n"
