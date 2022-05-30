module PNG.Container (
    PNGContainer (..),
    packPNG
) where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Binary.Put as BP

import PNG.Chunks

data PNGContainer = PNGContainer {
    ihdrChunks :: [IHDRChunk],
    idatChunks :: [IDATChunk]
}

packPNGHeader :: BLU.ByteString
packPNGHeader = BLU.fromString "\137\80\78\71\13\10\26\10"

serializePNGContainer :: PNGContainer -> BP.Put
serializePNGContainer (PNGContainer ihdrChunks idatChunks) = do
    BP.putLazyByteString packPNGHeader
    serializePNGChunks ihdrChunks
    serializePNGChunks idatChunks
    serializePNGChunks [IENDChunk]

packPNG :: PNGContainer -> BLU.ByteString
packPNG pngContainer = BP.runPut (serializePNGContainer pngContainer)