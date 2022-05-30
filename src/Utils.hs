module Utils (
    byteArrayToByteString,
    clearOrSetBit
) where

import qualified Data.ByteArray as BA
import qualified Data.Bits as DB
import qualified Data.Word as DW

byteArrayToByteString array = BA.pack $ BA.unpack array

clearOrSetBit :: DW.Word8 -> Int -> Maybe Bool -> DW.Word8
clearOrSetBit number bitNum Nothing = number
clearOrSetBit number bitNum (Just True) = DB.setBit number bitNum
clearOrSetBit number bitNum (Just False) = DB.clearBit number bitNum
