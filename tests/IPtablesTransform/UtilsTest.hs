{- Author: Stefan Selent -}
module IPtablesTransform.UtilsTest where

import Test.HUnit
import IPtablesTransform.Utils
import IPtablesTransform.Rule


test_toDict = [("-p","21"),("-s","sd")] @=? toDict' ["-p","21","-s","sd"]
test_toDictNeg = [("-p","!21"),("-s","!sd")] @=? toDict' ["!","-p","21","!","-s","sd"]
test_toDictEmpty = [] @=? toDict' []
test_toDictNoOptions = [] @=? toDict' ["p","21","sss","sd"]


test_isIntervalNegatedYes = True @=? isIntervalNegated "! 34"
test_isIntervalNegatedNo = False @=? isIntervalNegated "34"


test_rangeFromInterval = 101 @=? rangeFromInterval (30, 130)


test_checkIf2ByteSizeUpper = True @=? checkIf2ByteSize (2^16-1)
test_checkIf2ByteSizeLower = True @=? checkIf2ByteSize 0
test_checkIf2ByteSizeNot = False @=? checkIf2ByteSize (2^17)


test_checkIfByteSizeUpper = True @=? checkIfByteSize 255
test_checkIfByteSizeLower = True @=? checkIfByteSize 0
test_checkIfByteSizeNot = False @=? checkIfByteSize 512

test_replaceNthEmpty = [] @=? replaceNth 1 1 []
test_replaceNthIndexTooLow = [1,2,3] @=? replaceNth (-1) 1 [1,2,3] 
test_replaceNthIndexTooHigh = [1] @=? replaceNth 3 1 [1]
test_replaceNth = [1,2,0,4] @=? replaceNth 2 0 [1,2,3,4]

test_splitIn3Empty = ([], [], []) @=? splitIn3 ([] :: [Int])
test_splitIn3Single = ([], [0], []) @=? splitIn3 [0]
test_splitIn3Two = ([0], [1], []) @=? splitIn3 [0,1]
test_splitIn3 = ([0,1,2,3,4], [5], [6,7,8,9]) @=? splitIn3 [0,1,2,3,4,5,6,7,8,9]
test_splitIn3Odd = ([0,1,2,3], [4], [5,6,7,8]) @=? splitIn3 [0,1,2,3,4,5,6,7,8]