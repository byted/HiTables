module IPtablesTransform.SelectorTest where

import Test.HUnit

import IPtablesTransform.Datatypes
import IPtablesTransform.Selector
import IPtablesTransform.Rule


suitableTCPRule = (RawRule (RawBox (False, univS) (False, univD) (False, univSP) (False, univDP) (False, (6,6))) (Chain "DROP"))
suitableUDPRule = (RawRule (RawBox (False, univS) (False, univD) (False, univSP) (False, univDP) (False, (17,17))) (Chain "DROP"))
notSuitableTCP =  (RawRule (RawBox (False, univS) (False, univD) (False, univSP) (False, univDP) (True, (17,17))) (Chain "DROP"))

test_getRelevantBlocksEmpty = [] @=? getRelevantBlocks ("INPUT", [])
test_getRelevantBlocksCompleteRelevant = [(0, (0,99), rules)] @=? getRelevantBlocks ("INPUT", rules)
    where   rules = take 100 $ repeat suitableUDPRule
test_getRelevantBlocks = [(0,(0,10), init $ init rules)] @=? getRelevantBlocks ("INPUT", rules)
    where   rules = (take 11 $ repeat suitableUDPRule) ++
                [   (RawRule (RawBox (False, univS) (False, univD) (False, univSP) (False, univDP) (True, (6,6))) (Chain "DROP"))
                ,   suitableUDPRule ]
test_getRelevantBlocksNotSuitableBlock = [] @=? getRelevantBlocks ("OUTPUT", take 100 $ repeat notSuitableTCP)