module Utils (
    byteArrayToByteString,
    clearOrSetBit
) where

import qualified Data.ByteArray as BA
import qualified Data.Bits as DB
import qualified Data.Word as DW

-- Converts any type that implements ByteArrayAccess to a more useful ByteArray
byteArrayToByteString :: (BA.ByteArray a1, BA.ByteArrayAccess a2) => a2 -> a1
byteArrayToByteString array = BA.pack $ BA.unpack array

-- Clears or set a specific bit on a number
-- If no operation is specified, this operation is a no-op
clearOrSetBit :: DW.Word8 -> Int -> Maybe Bool -> DW.Word8
clearOrSetBit number bitNum Nothing = number
clearOrSetBit number bitNum (Just True) = DB.setBit number bitNum
clearOrSetBit number bitNum (Just False) = DB.clearBit number bitNum
