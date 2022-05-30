module PNG.Container (
    PNGContainer (..),
    packPNG
) where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Binary.Put as BP

import PNG.Chunks

-- Data types

-- A PNG file, consisting of IHDR and IDAT chunks.
data PNGContainer = PNGContainer {
    ihdrChunks :: [IHDRChunk],
    idatChunks :: [IDATChunk]
}

-- Functions

-- Packs a PNG header into a lazy UTF-8 ByteString.
packPNGHeader :: BLU.ByteString
packPNGHeader = BLU.fromString "\137\80\78\71\13\10\26\10"

-- Serializes a PNG file into a BP.Put.
serializePNGContainer :: PNGContainer -> BP.Put
serializePNGContainer (PNGContainer ihdrChunks idatChunks) = do
    BP.putLazyByteString packPNGHeader
    serializePNGChunks ihdrChunks
    serializePNGChunks idatChunks
    serializePNGChunks [IENDChunk]

-- Serializes a PNG file into a lazy UTF-8 ByteString.
packPNG :: PNGContainer -> BLU.ByteString
packPNG pngContainer = BP.runPut (serializePNGContainer pngContainer)