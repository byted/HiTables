{- Author: Stefan Selent -}
{-# LANGUAGE DeriveDataTypeable #-}

module IPtablesTransform.Datatypes
where

import Data.Typeable
import Data.Generics
import Data.Word
import Data.HashSet as H

{- Emitter -}
type ChainCommand = String
type RuleCommand = String
type JumpRule = (String, Rule)
type OriginalRule = (String, Rule)
data ChildSearchType = Linear | Binary
    deriving (Data, Typeable, Enum, Show, Read, Eq, Ord)
type ChainStats = (String, [(Int, Int, Int, Int)])

{- Rule -}
data Action = Chain String
    deriving (Show, Read, Eq)

data Chain = INPUT | OUTPUT | FORWARD
    deriving (Show, Read, Eq)

data RawRule = RawRule RawBox Action
    deriving (Eq, Show)
data RawBox = RawBox    {   raw_src_addrs :: RawInterval   -- 32 Bit
                        ,   raw_dst_addrs :: RawInterval   -- 32 Bit
                        ,   raw_src_ports :: RawInterval   -- 16 Bit
                        ,   raw_dst_ports :: RawInterval
                        ,   raw_prot :: RawInterval        -- 8 Bit
                        } deriving (Eq, Show)
type RawInterval = (Bool, Interval)     -- Bool indicates whether Interval is negated (True) or not (False)

type RawChain = (String, [RawRule])


type ChainRule = (String, Rule)


data Rule = Rule Box Action
    deriving (Eq, Show)
data Box = Box  {   src_addrs :: Interval   -- 32 Bit
                ,   dst_addrs :: Interval   -- 32 Bit
                ,   src_ports :: Interval   -- 16 Bit
                ,   dst_ports :: Interval
                ,   prot :: Interval        -- 8 Bit
                } deriving (Eq, Show)
type Interval = (Word32, Word32)
data DimensionName = IPSrc | IPDst | PortSrc | PortDst | Protocol
    deriving (Enum, Show, Read, Eq, Ord)

{- Parser -}
data Error = MalformedRawRule
            | EmptyLine
            | Comment
            | Config
            | ActionMissing
            | SubnetParseError
            | ProtocolParseError
            | PortsParseError
            | ActionParseError
    deriving (Show, Read, Eq)

type ErrorInLine = (Int, Error)

{- Tree -}
data Tree =   EmptyTree 
            | Node  {   filterBox :: Box
                    ,   rules :: [Rule]
                    ,   children :: [Tree]
                    }  
    deriving (Eq)

{- TransformationEngine -} 
data DimensionAlgorithmName =
        MinMaxRulesPerChild
    |   MinSM
    |   MaxDistinctRules
    |   MaxDistinctRulePairwise
    deriving (Data, Typeable, Show, Eq)

type DistinctRules = HashSet(String, Int)
type DimensionAlgorithm = (Tree -> (Int -> Int) -> DistinctRules -> Tree)
type Spmf = (Int -> Int)

{- Selector -}

type Offset = (Int, Int)
type Block = (Int, Offset, [RawRule])