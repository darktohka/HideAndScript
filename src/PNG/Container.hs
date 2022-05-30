module PNG.Container (
    PNGContainer (..),
    packPNG,
    writePNG,
    createRgbaPNG,
    randomPNGImage
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as BP
import qualified Data.Word as DW

import PNG.Chunks

-- Data types

-- A PNG file, consisting of IHDR and IDAT chunks.
data PNGContainer = PNGContainer {
    ihdrChunks :: [IHDRChunk],
    idatChunks :: [IDATChunk]
}

-- Functions

-- Packs a PNG header into a lazy UTF-8 ByteString.
packPNGHeader :: BL.ByteString
packPNGHeader = BL.pack [137, 80, 78, 71, 13, 10, 26, 10]

-- Serializes a PNG file into a BP.Put.
serializePNGContainer :: PNGContainer -> BP.Put
serializePNGContainer (PNGContainer ihdrChunks idatChunks) = do
    BP.putLazyByteString packPNGHeader
    serializePNGChunks ihdrChunks
    serializePNGChunks idatChunks
    serializePNGChunks [IENDChunk]

-- Serializes a PNG file into a lazy UTF-8 ByteString.
packPNG :: PNGContainer -> BL.ByteString
packPNG pngContainer = BP.runPut (serializePNGContainer pngContainer)

-- Writes a PNG container into a file.
writePNG :: PNGContainer -> FilePath -> IO ()
writePNG pngContainer filePath = BL.writeFile filePath $ packPNG pngContainer

-- Creates an RGBA PNG from a width, height and pixels
createRgbaPNG :: DW.Word32 -> DW.Word32 -> [PNGPixel] -> PNGContainer
createRgbaPNG width height pixels = PNGContainer [defaultIHDR width height] [IDATChunk width pixels]

-- Creates a random RGBA PNG with a given width and height.
randomPNGImage :: DW.Word32 -> DW.Word32 -> Int -> PNGContainer
randomPNGImage width height seed = createRgbaPNG width height (randomPixels (width * height) seed)
