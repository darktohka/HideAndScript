module PNG.Chunks (
    IPNGChunk (..),
    IHDRChunk (..),
    IDATChunk (..),
    IENDChunk (..),
    PNGPixel (..),
    randomPixel,
    randomPixels,
    defaultIHDR,
    serializePNGChunks
) where

import qualified Data.Digest.CRC32 as CRC
import qualified Codec.Compression.Zlib as Z
import qualified Data.Binary.Put as BP
import qualified Data.Word as DW
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Char8 as BC
import qualified System.Random as SR

import PNG.Constants

-- Data types

-- A class representing a PNG chunk.
-- All instances of this class must implement the
-- chunkName and chunkData functions.
class IPNGChunk chunk where
    -- Returns the name of this chunk.
    chunkName :: chunk -> String
    -- Returns the serialized data of this chunk.
    chunkData :: chunk -> BL.ByteString

-- Represents an IHDR chunk.
-- IHDR chunks contain width, height and color type data.
data IHDRChunk = IHDRChunk {
    width :: DW.Word32,
    height :: DW.Word32,
    bitDepth :: BitDepth,
    colorType :: ColorType,
    compressionMethod :: CompressionMethod,
    filterMethod :: FilterMethod,
    interlaceMethod :: InterlaceMethod
}

-- Represents an RGBA8 pixel.
data PNGPixel = PNGPixel {
    r :: DW.Word8,
    g :: DW.Word8,
    b :: DW.Word8,
    a :: DW.Word8
}

-- Represents an IDAT chunk.
-- IDAT chunks contain inflated (Zlib) image data.
data IDATChunk = IDATChunk {
    lineWidth :: DW.Word32,
    pixels :: [PNGPixel]
}

-- Represents an IEND chunk.
-- IEND chunks are completely empty
-- and only serve as a hint to the PNG parser.
data IENDChunk = IENDChunk {

}

-- Helper methods

-- Calculates the CRC of a chunk by CRCing its name and its contents.
chunkCRC :: (CRC.CRC32 a1, CRC.CRC32 a2) => a2 -> a1 -> DW.Word32
chunkCRC name byteData = CRC.crc32Update (CRC.crc32 name) byteData

-- Serializes an entire PNG chunk into a BP.Put.
-- See https://libpng.org/pub/png/spec/1.2/PNG-Structure.html
serializeChunk :: IPNGChunk a => a -> BP.Put
serializeChunk chunk = do
    -- Chunk length
    BP.putWord32be $ fromIntegral $ BL.length byteData
    -- Chunk name
    BP.putLazyByteString name
    -- Chunk data
    BP.putLazyByteString byteData
    -- Chunk CRC
    BP.putWord32be crc
    where
        name = BLU.fromString $ chunkName chunk
        byteData = chunkData chunk
        crc = chunkCRC name byteData

-- Serializes an entire PNG chunk into a lazy ByteString.
packChunk :: IPNGChunk a => a -> BL.ByteString
packChunk chunk = BP.runPut $ serializeChunk chunk

-- Serializes a list of PNG chunks into a BP.Put.
serializePNGChunks :: IPNGChunk a => [a] -> BP.Put
serializePNGChunks [] = return ()
serializePNGChunks (chunk : chunks) = do
    serializeChunk chunk
    serializePNGChunks chunks

-- Serializes an IHDR chunk into a BP.Put.
-- See: https://w3.org/TR/PNG-Chunks.html
serializeIHDRChunk :: IHDRChunk -> BP.Put
serializeIHDRChunk (IHDRChunk width height bitDepth colorType compressionMethod filterMethod interlaceMethod) = do
    BP.putWord32be width
    BP.putWord32be height
    BP.putWord8 $ fromBitDepth bitDepth
    BP.putWord8 $ fromColorType colorType
    BP.putWord8 $ fromCompressionMethod compressionMethod
    BP.putWord8 $ fromFilterMethod filterMethod
    BP.putWord8 $ fromInterlaceMethod interlaceMethod

-- Serializes an IHDR chunk into a lazy ByteString.
packIHDRContents :: IHDRChunk -> BL.ByteString
packIHDRContents chunk = BP.runPut (serializeIHDRChunk chunk)

-- Serializes an RGBA8 PNGPixel into a BP.Put.
serializePNGPixel :: PNGPixel -> BP.Put
serializePNGPixel (PNGPixel r g b a) = do
    BP.putWord8 r
    BP.putWord8 g
    BP.putWord8 b
    BP.putWord8 a

-- Serializes a list of PNGPixels into a BP.Put.
-- Please note, that according to the PNG specification,
-- a scanline byte should be added at the start of each row.
serializePNGPixels :: [PNGPixel] -> DW.Word32 -> DW.Word32 -> BP.Put
serializePNGPixels [] width i = return ()
serializePNGPixels (pixel : pixels) lineWidth 0 = do
    -- Insert scanline byte: 0 - no filter
    BP.putWord8 0
    serializePNGPixel pixel
    serializePNGPixels pixels lineWidth (lineWidth - 1)
serializePNGPixels (pixel : pixels) lineWidth i = do
    serializePNGPixel pixel
    serializePNGPixels pixels lineWidth (i - 1)

-- Serializes an IDAT chunk into a BP.Put.
packIDATContents :: IDATChunk -> BL.ByteString
packIDATContents (IDATChunk lineWidth pixels) = Z.compress $ BP.runPut $ serializePNGPixels pixels lineWidth 0

-- Creates a default IHDR header from just specifying a width and a height.
defaultIHDR :: DW.Word32 -> DW.Word32 -> IHDRChunk
defaultIHDR width height = IHDRChunk width height EightBit TrueColorAlpha Deflate NoFilter NoInterlace

-- Random methods

-- Generates a random pixel, given a seed.
randomPixel :: Int -> PNGPixel
randomPixel seed = PNGPixel (r :: DW.Word8) (g :: DW.Word8) (b :: DW.Word8) 255
    where
        gen = SR.mkStdGen seed
        (r, genA) = SR.uniformR (0, 255) gen
        (g, genB) = SR.uniformR (0, 255) genA
        (b, _) = SR.uniformR (0, 255) genB

-- Generates N random pixels, given a seed.
randomPixels 0 seed = []
randomPixels num seed = [randomPixel seed] ++ (randomPixels (num - 1) (seed + 1))

-- Instances

-- An IDAT chunk.
instance IPNGChunk IDATChunk where
    chunkName _ = "IDAT"
    chunkData chunk = packIDATContents chunk

-- An IHDR chunk.
instance IPNGChunk IHDRChunk where
    chunkName _ = "IHDR"
    chunkData chunk = packIHDRContents chunk

-- An IEND chunk.
instance IPNGChunk IENDChunk where
    chunkName _ = "IEND"
    chunkData _ = BL.empty
