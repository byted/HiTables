{- Author: Stefan Selent -}
module IPtablesTransform.ParserTest where

import Test.HUnit
import IPtablesTransform.Datatypes
import IPtablesTransform.Parser
import IPtablesTransform.Utils
import IPtablesTransform.Rule


test_getAction = Right (Chain "DROP") @=? getAction [("-j", "DROP")]
test_getActionErr = Left ActionParseError @=? getAction [("-j", "! DROP")]
test_getActionErrMissing = Left ActionMissing @=? getAction [("-s", "! DROP")]



test_parseIPtablesEmpty = Right [] @=? parseIPtables ""
test_parseIPtablesIsStable = res @=? parseIPtables input
    where   res = Right [("FORWARD",[defaultRawRule]),("INPUT",[defaultRawRule, RawRule defaultRawBox (Chain "ACCEPT")])]
            input = "-A INPUT -j DROP\n-A FORWARD -j DROP\n-A INPUT -j ACCEPT"

test_parseIPtablesSingleChainType = Right [("OUTPUT",[defaultRawRule])] @=? parseIPtables "-A OUTPUT -j DROP"


test_parseRawRuleFromWordsDefault = Right ("INPUT", defaultRawRule) @=? parseRawRuleFromWords ["-A", "INPUT", "-j", "DROP"]
test_parseRawRuleFromWordsComplete = Right ("INPUT", rule) @=? parseRawRuleFromWords    ["-A"
                                                                        ,"INPUT"
                                                                        ,"-s", "192.168.0.2"
                                                                        ,"-d", "192.168.2.0"
                                                                        ,"-p", "udp"
                                                                        ,"--sport", "21"
                                                                        ,"--dport", "22"
                                                                        ,"-j", "ACCEPT" ]
    where   rule = RawRule (RawBox (False,s) (False,d) (False,(21, 21)) (False,(22, 22)) (False,(17, 17))) (Chain "ACCEPT")
            Right s = subnetParser "192.168.0.2"
            Right d = subnetParser "192.168.2.0"
test_parseRawRuleFromWordsComment = Left Comment @=? parseRawRuleFromWords ["#", "-A", "INPUT"]
test_parseRawRuleFromWordsEmpty = Left EmptyLine @=? parseRawRuleFromWords []
test_parseRawRuleFromWordsError = Left MalformedRawRule @=? parseRawRuleFromWords ["dsf", "-A", "INPUT"]
test_parseRawRuleFromWordsNeg = Right ("FORWARD", rule) @=? parseRawRuleFromWords ["-A", "FORWARD", "-p", "!", "21", "-j", "ACCEPT"]
    where   rule = RawRule (RawBox (False,(0, 2^32-1)) (False,(0, 2^32-1)) (False,(0, 2^16-1)) (False,(0, 2^16-1)) (True,(21, 21))) (Chain "ACCEPT")


test_subnetParser = Right (0, 134217727) @=? subnetParser "1.2.3.4/5"
test_subnetParserNoMask = Right (16909060, 16909060) @=? subnetParser "1.2.3.4"
test_subnetParserErrMask = Left SubnetParseError @=? subnetParser "1.2.3.4/256"
test_subnetParserErrIP = Left SubnetParseError @=? subnetParser "1.2.4907.4/5"
test_subnetParserEmptyInput = Left SubnetParseError @=? subnetParser ""
test_subnetParserIPTooLong = Left SubnetParseError @=? subnetParser "1.2.3.4.5.6/5"
test_subnetParserStrangeMask = Left SubnetParseError @=? subnetParser "1.2.3.4.5./5/5"


test_protocolParserTcp = Right (6,6) @=? protocolParser "tcp"
test_protocolParserUDP = Right (17,17) @=? protocolParser "UDP"
test_protocolParserNbr = Right (6,6) @=? protocolParser "6"
test_protocolParserEmptyInput = Left ProtocolParseError @=? protocolParser ""
test_protocolParserBadNbr = Left ProtocolParseError @=? protocolParser "256"
test_protocolParserBadString = Left ProtocolParseError @=? protocolParser "ufp"


test_portsParserSinglePort = Right (21, 21) @=? portsParser "21"
test_portsParserRange = Right (21, 112) @=? portsParser "21:112"
test_portsParser = Left PortsParseError @=? portsParser ""
test_portsParserOutOfRange = Left PortsParseError @=? portsParser "21:1123423545423534"
test_portsParserInvalidRange = Left PortsParseError @=? portsParser "21:112:67"
test_portsParserInvalidInput = Left PortsParseError @=? portsParser "sd43"


test_subnetToInterval = (0, 134217727) @=? subnetToInterval 1 2 3 4 5




test_getLegitRawRules = res @=? getLegitRawRules [Right ("first", defaultRawRule), Right ("second", defaultRawRule)]
    where   res = [("first", defaultRawRule), ("second", defaultRawRule)]

test_toChainDictStable = res @=? toChainDict    [   [("b", acceptRule), ("b", defaultRawRule)]
                                                ,   [("a", defaultRawRule), ("a", acceptRule)]
                                                ]
    where   res =   [   ("b", [acceptRule, defaultRawRule])
                    ,   ("a", [defaultRawRule, acceptRule])]
            acceptRule = RawRule defaultRawBox (Chain "ACCEPT")
