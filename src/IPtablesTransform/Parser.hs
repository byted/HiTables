{- Author: Stefan Selent -}
module IPtablesTransform.Parser
where
  
import Data.Word
import Data.Bits
import Data.List.Split
import Data.List
import Debug.Trace

import IPtablesTransform.Datatypes
import IPtablesTransform.Rule
import IPtablesTransform.Utils



{-
Takes the content of an iptables config file as a string and parses it.
Returns a list of lists of pairs containing the chain name and the parsed rules.
-}
parseIPtables :: String -> Either ErrorInLine [RawChain]
parseIPtables str = case checkForErrors parsedLines of
                        Just err -> Left err
                        Nothing -> Right (toChainDict . groupByChain . getLegitRawRules $ parsedLines)
    where   ruleAsWordList = map words (lines str)
            parsedLines = map parseRawRuleFromWords ruleAsWordList
            groupByChain xs = groupBy (\(x,_) (y,_) -> x == y) $ sortBy (\(x,_) (y,_) -> x `compare` y) xs -- is stable (chain-wise)

{-
Takes a list of lists containing (chain, rule) pairs and converts them into a dictionary:
[                                                   [
    [(chain1, rule1), (chain1, rule2), ...],    =>      (chain1, [rule1, rule2]),
    [(chain2, rule1), (chain2, rule2), ...]             (chain2, [rule1, rule2])
]                                                   ]
-}
toChainDict :: [[(String, RawRule)]] -> [RawChain]
toChainDict xs = map createKeyValPair xs
    where   createKeyValPair chainRulePairs =   let key = (fst . head $ chainRulePairs)
                                                    val = foldr (\(chainName, rule) acc -> rule:acc) [] chainRulePairs
                                                    in  (key, val)               


{-
Checks a given list of chain-rawRules elements for errors
and returns the line number and error-type 
-}
-- rewrite with foldl ?
checkForErrors :: [Either Error (String, RawRule)] -> Maybe ErrorInLine
checkForErrors xs = checkForFails' 1 xs
    where   checkForFails' _ [] = Nothing
            checkForFails' line (x:xs) =    case x of
                                                Left EmptyLine -> checkForFails' (line + 1) xs
                                                Left Comment -> checkForFails' (line + 1) xs
                                                Left Config -> checkForFails' (line + 1) xs
                                                Left err -> Just(line, err)
                                                otherwise -> checkForFails' (line + 1) xs

{-
Extracts legit chain-rawRules-pairs from a list of given
chain-rawRules-pairs
    >>> foldr to maintan stability <<<
-}
getLegitRawRules :: [Either Error (String, RawRule)] -> [(String, RawRule)]
getLegitRawRules xs = foldr recFct [] xs
    where recFct x acc =    case x of
                                Right rule -> rule:acc
                                Left _ -> acc

{-
Takes a line from the file as list of words, parses options and checks for errors
returns either an error or a tupel containing the chain and rule
-}
parseRawRuleFromWords :: [String] -> Either Error (String, RawRule)
parseRawRuleFromWords ("-A":chain:rest) =   
    case action of
        Left err -> Left err
        Right noErrAction ->
            case src_addrs of
                Left err -> Left err
                Right noErrSrc_addrs ->
                    case dst_addrs of
                        Left err -> Left err
                        Right noErrDst_addrs -> 
                            case src_ports of
                            Left err -> Left err
                            Right noErrSrc_ports ->
                                case dst_ports of
                                    Left err -> Left err
                                    Right noErrDst_ports -> 
                                        case prot of
                                            Left err -> Left err
                                            Right noErrProt -> Right (chain, (RawRule newBox noErrAction))
                                                where newBox = RawBox noErrSrc_addrs noErrDst_addrs noErrSrc_ports noErrDst_ports noErrProt
    where   rawRawRuleDict = toDict' rest
            src_addrs = getInterval "-s" subnetParser (0, 4294967295) rawRawRuleDict
            dst_addrs = getInterval "-d" subnetParser (0, 4294967295) rawRawRuleDict
            src_ports = getInterval "--sport" portsParser (0, 65535) rawRawRuleDict
            dst_ports = getInterval "--dport" portsParser (0, 65535) rawRawRuleDict
            prot = getInterval "-p" protocolParser (0, 2^8-1) rawRawRuleDict
            action = getAction rawRawRuleDict            
parseRawRuleFromWords [] = Left EmptyLine
parseRawRuleFromWords (('#':_):_) = Left Comment
parseRawRuleFromWords (('*':_):_) = Left Config
parseRawRuleFromWords ((':':_):_) = Left Config
parseRawRuleFromWords ("COMMIT":_) = Left Config
parseRawRuleFromWords _ = Left MalformedRawRule

{-  
Meta-function
Takes a search string, a parser, a default intervall
and the options dictionary and puts everything together and handles negation
-}
getInterval :: String -> (String -> Either Error Interval) -> Interval -> [(String, String)] -> Either Error RawInterval
getInterval searchStr parser defaultInterval xs = case res of
                                                    Nothing -> Right (False, defaultInterval)
                                                    Just ('!':str) -> buildRawInterval (parser str) True 
                                                    Just str -> buildRawInterval (parser str) False   
    where   res = lookup searchStr xs
            buildRawInterval parseRes neg =  case parseRes of
                                        Left err -> Left err
                                        Right iv -> Right (neg, iv)


getAction :: [(String, String)] -> Either Error Action
getAction xs = case res of 
                    Nothing -> Left ActionMissing
                    Just ('!':' ':str) -> Left ActionParseError
                    Just str -> Right (Chain action)
    where   res = lookup "-j" xs
            Just action = res

protocolParser :: String -> Either Error Interval
protocolParser "" = Left ProtocolParseError
protocolParser str =   case res == Nothing of
                                True -> parseNum checkIfByteSize str
                                False -> Right (x, x)
    where   res = lookup str protocolNameToNum
            Just x = res

subnetParser :: String -> Either Error Interval
subnetParser "" = Left SubnetParseError
subnetParser str = let  subnetAsList = addMaskIfNeeded $ map (\x -> read x :: Word32) splittedIntoStrings
                        splittedIntoStrings = init dotSplitted ++ splitOn "/" (last dotSplitted)
                        dotSplitted = splitOn "." str
                        checkIfLegitSubnet xs = if validRange && validLength then True else False
                            where   validRange = foldl (\acc x -> if checkIfByteSize x then acc else False) True xs
                                    validLength = length xs == 5
                        addMaskIfNeeded xs = if length xs == 4 then xs ++ [32] else xs
                        in  if checkIfLegitSubnet subnetAsList then 
                                let (a:b:c:d:mask:[]) = subnetAsList
                                    in Right (subnetToInterval a b c d mask)
                            else
                                Left SubnetParseError

portsParser :: String -> Either Error Interval
portsParser "" = Left PortsParseError
portsParser str =  case length splitted of
                            1 -> createInterval (splitted ++ splitted)
                            2 -> createInterval splitted
                            otherwise -> Left PortsParseError
    where   splitted = splitOn ":" str
            createInterval (a:b:[]) =   case readMaybe a :: Maybe Word32 of
                                            Nothing -> Left PortsParseError
                                            Just lb ->  case readMaybe b :: Maybe Word32 of
                                                            Nothing -> Left PortsParseError
                                                            Just ub ->  if checkIf2ByteSize lb && checkIf2ByteSize ub then
                                                                            Right (lb, ub)
                                                                        else
                                                                            Left PortsParseError

parseNum :: (Word32 -> Bool) -> String -> Either Error Interval           
parseNum sizeChecker str =  case readMaybe str :: Maybe Word32 of
                                Nothing -> Left ProtocolParseError
                                Just nbr -> if sizeChecker nbr then
                                                Right (nbr, nbr)
                                            else
                                                Left ProtocolParseError     



subnetToInterval :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Interval
subnetToInterval a b c d mask32 =   let mask = fromIntegral mask32
                                        upperBound = (2^(32-mask)-1) .|. lowerBound
                                        lowerBound = ipAsInt `shiftR` (32 - mask) `shift` (32 - mask)
                                        ipAsInt = (a `shift` 24) .|. (b `shift` 16) .|. (c `shift` 8) .|. d
                                        in (lowerBound, upperBound)                                                                       