{- Author: Stefan Selent -}
module IPtablesTransform.RuleTest where

import Test.HUnit
import IPtablesTransform.Datatypes
import IPtablesTransform.Parser (subnetToInterval, subnetParser)
import IPtablesTransform.Utils
import IPtablesTransform.Rule

test_isProtocolNegated = True @=? isProtocolNegated (RawRule (RawBox (False, univS) (False, univD) (False, univSP) (False, univDP) (True, (8,8))) (Chain "DROP"))
test_isProtocolNegatedFalse = False @=? isProtocolNegated defaultRawRule

test_isUDP = True @=? isUDP (RawRule (RawBox (False, univS) (False, univD) (False, univSP) (False, univDP) (True, (17,17))) (Chain "DROP"))
test_isUDPFalse = False @=? isUDP defaultRawRule

test_isTCP = True @=? isTCP (RawRule (RawBox (False, univS) (False, univD) (False, univSP) (False, univDP) (True, (6,6))) (Chain "DROP"))
test_isTCPFalse = False @=? isTCP defaultRawRule


test_boxesTouching = True @=? boxesTouching defaultBox defaultBox 0
test_boxesTouchingNotTouching = False @=? boxesTouching b1 b2 2
    where   b1 = Box univS univD (0,2) univDP univP
            b2 = Box univS univD (3,10) univDP univP
test_boxesTouchingBorderCase = True @=? boxesTouching b1 b2 2
    where   b1 = Box univS univD (0,2) univDP univP
            b2 = Box univS univD (2,10) univDP univP


test_ruleNormalizer = [defaultRule] @=? ruleNormalizer defaultRawRule
test_ruleNormalizerAllNeg = 2^5 @=? length (ruleNormalizer fullNegRawRule)
test_ruleNormalizerOneNeg = [r1, r2] @=? ruleNormalizer rr
    where   rr = RawRule    (RawBox (False, univS)
                                    (False, univD)
                                    (False, univSP)
                                    (False, univDP)
                                    (True, (21, 21))
                            ) (Chain "ACCEPT")
            r1 = Rule   (Box univS univD univSP univDP (0,20)) (Chain "ACCEPT")
            r2 = Rule   (Box univS univD univSP univDP (22,2^8-1)) (Chain "ACCEPT")
test_ruleNormalizerInsane = [] @=? ruleNormalizer rr
    where   rr = RawRule    (RawBox (False, univS)
                                    (False, univD)
                                    (False, univSP)
                                    (False, univDP)
                                    (True, univP)
                            ) (Chain "DROP")

test_getIntervalByIndexProt = (0,36) @=? getIntervalByIndex 4 (Box univS univD univSP univDP (0,36))
test_getIntervalByIndexIPSrc = (5,5) @=? getIntervalByIndex 0 (Box (5,5) univD univSP univDP univP)
test_getIntervalByIndexPortSrc = (5,5) @=? getIntervalByIndex 2 (Box univS univD (5,5) univDP univP)


test_rangeFromIntervalBad = 0 @=? rangeFromInterval (2,1)
test_rangeFromInterval = 2 @=? rangeFromInterval (1,2)

test_interval2IPrange = "0.0.0.0-1.2.3.4" @=? interval2IPrange (0, fst $ subnetToInterval 1 2 3 4 32)

test_interval2Range =  "45:3345" @=? interval2Range (45, 3345)

test_word32ToIPNull = "0.0.0.0" @=? word32ToIP 0 
test_word32ToIP = "1.2.3.4" @=? word32ToIP (fst $ subnetToInterval 1 2 3 4 32)



test_removeShadowedRules = removeShadowedRules [r1, r2, r3] @?= [r1, r2]
    where   r1 = Rule (Box (0, 0) (5, 5) univSP (0, 0) (16, 16)) $ Chain "ACCEPT"
            r2 = Rule (Box (10, 20) (100, 200) univSP (0, 5000) (7, 7)) $ Chain "ACCEPT"
            r3 = Rule (Box (10, 19) (100, 100) (0, 0) (0, 0) (7, 7)) $ Chain "DROP"

test_removeShadowedRulesBrutal = removeShadowedRules [r1, r2, r3, r4, r5, r6] @?= [r1, r4, r5]
    where   r1 = Rule (Box (10, 21) (99, 201) univSP (0, 5001) (7, 16)) $ Chain "ACCEPT"
            r2 = Rule (Box (10, 20) (100, 200) univSP (0, 5000) (7, 7)) $ Chain "ACCEPT"
            r3 = Rule (Box (10, 19) (100, 100) (0, 0) (0, 0) (7, 7)) $ Chain "DROP"
            r4 = Rule (Box (10, 19) (500, 500) (0, 0) (0, 0) (7, 7)) $ Chain "DROP"
            r5 = Rule (Box (31, 32) (400, 401) (0, 0) (0, 0) (7, 7)) $ Chain "ACCEPT"
            r6 = Rule (Box (32, 32) (400, 400) (0, 0) (0, 0) (7, 7)) $ Chain "DROP"

test_removeRedundantRules = removeRedundantRules [r0, r1, r2, r3] @?= [r0, r2, r3]
    where   r0 = defaultRule
            r1 = Rule (Box (10, 20) (100, 200) (45, 76) (0, 5000) (7, 7)) $ Chain "ACCEPT"
            r2 = Rule (Box (10, 20) (0, 400) univSP (0, 5001) (7, 7)) $ Chain "ACCEPT"
            r3 = Rule (Box (10, 20) (0, 400) univSP (0, 5001) (7, 7)) $ Chain "DROP"

test_removeShadowing1 = removeShadowing r1 [r1, r2, r3] [] @?= [r2, r3]
    where   r1 = Rule (Box (0, 0) (5, 5) univSP (0, 0) (16, 16)) $ Chain "ACCEPT"
            r2 = Rule (Box (10, 20) (100, 200) univSP (0, 5000) (7, 7)) $ Chain "ACCEPT"
            r3 = Rule (Box (10, 19) (100, 100) (0, 0) (0, 0) (7, 7)) $ Chain "DROP"

test_removeShadowing2 = removeShadowing r1 [r1] [] @?= []
    where   r1 = Rule (Box (0, 0) (5, 5) univSP (0, 0) (16, 16)) $ Chain "ACCEPT"
            r2 = Rule (Box (10, 20) (100, 200) univSP (0, 5000) (7, 7)) $ Chain "ACCEPT"
            r3 = Rule (Box (10, 19) (100, 100) (0, 0) (0, 0) (7, 7)) $ Chain "DROP"

test_removeShadowing3 = removeShadowing r2 [r2, r3] [] @?= []
    where   r1 = Rule (Box (0, 0) (5, 5) univSP (0, 0) (16, 16)) $ Chain "ACCEPT"
            r2 = Rule (Box (10, 20) (100, 200) univSP (0, 5000) (7, 7)) $ Chain "ACCEPT"
            r3 = Rule (Box (10, 19) (100, 100) (0, 0) (0, 0) (7, 7)) $ Chain "DROP"


test_rawInterval2Protocol = "17" @=? rawInterval2Protocol (raw_prot b)
    where   b = RawBox (False,s) (False,d) (False,(21, 21)) (False,(22, 22)) (False,(17, 17))
            Right s = subnetParser "192.168.0.2"
            Right d = subnetParser "192.168.2.0"