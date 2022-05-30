{-# LANGUAGE RecordWildCards #-}

module StreamHider (
    readImageFromFile,
    decodeStringFromImage,
    encodeStringIntoImage,
    calculateBandwidth,
    saveHiddenImage
) where

import qualified Codec.Picture as CP
import qualified Codec.Picture.Types as CPT
import qualified Control.Monad.ST as ST
import qualified Data.BitString as BTS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Bits as DB
import qualified Data.Vector.Storable as V
import qualified Utils as U
import Codec.Picture (savePngImage)

readImageFromFile :: FilePath -> IO (Maybe (CP.Image CP.PixelRGBA8))
readImageFromFile filename = do
    image <- CP.readImage filename

    case image of
      Left err -> return Nothing
      Right img -> return $ Just $ CP.convertRGBA8 img

decodeStringFromImage :: CP.Image CP.PixelRGBA8 -> BL.ByteString
decodeStringFromImage image@CP.Image {..} = BTS.realizeBitStringLazy (BTS.fromList (decodeBitsFromImage 0 0 []))
    where
        (CP.Image imageWidth imageHeight imageData) = image

        decodeBitsFromImage width height res
            | width >= imageWidth   = decodeBitsFromImage 0 (height + 1) res
            | height >= imageHeight = res
            | otherwise = decodeBitsFromImage (width + 1) height (res ++ [DB.testBit r 0, DB.testBit g 0, DB.testBit b 0])
                where
                    (CP.PixelRGBA8 r g b _) = CP.pixelAt image width height

calculateBandwidth :: Integral a => a -> a -> a
calculateBandwidth imageWidth imageHeight = (imageWidth * imageHeight * 3) `div` 8

actualEncodeStringIntoImage :: CP.Image CP.PixelRGBA8 -> BS.ByteString -> CP.Image CP.PixelRGBA8
actualEncodeStringIntoImage img@CP.Image {..} hiddenContent = ST.runST $ do
  mutableImage <- CPT.newMutableImage imageWidth imageHeight
  let hiddenBits = V.fromList (BTS.toList (BTS.bitString hiddenContent))

  let writePixelAt x y bitIndex
        | x >= imageWidth  = writePixelAt 0 (y + 1) bitIndex
        | y >= imageHeight = CPT.unsafeFreezeImage mutableImage
        | otherwise = do
            CP.writePixel mutableImage x y newPixel
            writePixelAt (x + 1) y (bitIndex + 3)
        where
            (CP.PixelRGBA8 r g b a) = CP.pixelAt img x y
            newR = U.clearOrSetBit r 0 (hiddenBits V.!? (bitIndex))
            newG = U.clearOrSetBit g 0 (hiddenBits V.!? (bitIndex + 1))
            newB = U.clearOrSetBit b 0 (hiddenBits V.!? (bitIndex + 2))
            newPixel = (CP.PixelRGBA8 newR newG newB a)

  writePixelAt 0 0 0

encodeStringIntoImage :: CP.Image CP.PixelRGBA8 -> BS.ByteString -> Maybe (CP.Image CP.PixelRGBA8)
encodeStringIntoImage img@CP.Image {..} hiddenContent = do
  let bandwidth = calculateBandwidth imageWidth imageHeight

  case bandwidth >= (BS.length hiddenContent) of
      True -> Just (actualEncodeStringIntoImage img hiddenContent)
      False -> Nothing

actualSaveHiddenImage :: Maybe (CP.Image CP.PixelRGBA8) -> FilePath -> IO Bool
actualSaveHiddenImage (Just image) outputFilename = do
    CP.savePngImage outputFilename (CP.ImageRGBA8 image)
    return True
actualSaveHiddenImage Nothing _ = return False

saveHiddenImage :: (CP.Image CP.PixelRGBA8) -> BS.ByteString -> FilePath -> IO Bool
saveHiddenImage image hiddenContent outputFilename = actualSaveHiddenImage (encodeStringIntoImage image hiddenContent) outputFilename
