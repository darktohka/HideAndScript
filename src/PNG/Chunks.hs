module PNG.Chunks (
    IPNGChunk (..),
    IHDRChunk (..),
    IDATChunk (..),
    IENDChunk (..),
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

import PNG.Constants

-- Data

class IPNGChunk chunk where
    chunkName :: chunk -> String
    chunkData :: chunk -> BL.ByteString

data IHDRChunk = IHDRChunk {
    width :: DW.Word32,
    height :: DW.Word32,
    bitDepth :: BitDepth,
    colorType :: ColorType,
    compressionMethod :: CompressionMethod,
    filterMethod :: FilterMethod,
    interlaceMethod :: InterlaceMethod
}

data IDATChunk = IDATChunk {

}

data IENDChunk = IENDChunk {

}

-- Helper methosd

chunkCRC :: (CRC.CRC32 a1, CRC.CRC32 a2) => a2 -> a1 -> DW.Word32
chunkCRC name byteData = CRC.crc32Update (CRC.crc32 name) byteData

serializeChunk :: IPNGChunk a => a -> BP.Put
serializeChunk chunk = do
    BP.putWord32be $ fromIntegral $ BLU.length byteData
    BP.putLazyByteString name
    BP.putLazyByteString byteData
    BP.putWord32be crc
    where
        name = BLU.fromString $ chunkName chunk
        byteData = chunkData chunk
        crc = chunkCRC name byteData

packChunk :: IPNGChunk a => a -> BL.ByteString
packChunk chunk = BP.runPut $ serializeChunk chunk

serializePNGChunks :: IPNGChunk a => [a] -> BP.Put
serializePNGChunks [] = return ()
serializePNGChunks (chunk : chunks) = do
    serializeChunk chunk
    serializePNGChunks chunks

serializeIHDRChunk :: IHDRChunk -> BP.Put
serializeIHDRChunk (IHDRChunk width height bitDepth colorType compressionMethod filterMethod interlaceMethod) = do
    BP.putWord32be width
    BP.putWord32be height
    BP.putWord8 $ fromBitDepth bitDepth
    BP.putWord8 $ fromColorType colorType
    BP.putWord8 $ fromCompressionMethod compressionMethod
    BP.putWord8 $ fromFilterMethod filterMethod
    BP.putWord8 $ fromInterlaceMethod interlaceMethod

packIHDRContents :: IHDRChunk -> BL.ByteString
packIHDRContents chunk = BP.runPut (serializeIHDRChunk chunk)

defaultIHDR :: DW.Word32 -> DW.Word32 -> IHDRChunk
defaultIHDR width height = IHDRChunk width height SixteenBit TrueColorAlpha Deflate NoFilter NoInterlace

-- Instances

instance IPNGChunk IDATChunk where
    chunkName _ = "IDAT"
    chunkData chunk = BLU.fromString ""

instance IPNGChunk IHDRChunk where
    chunkName _ = "IHDR"
    chunkData chunk = packIHDRContents chunk

instance IPNGChunk IENDChunk where
    chunkName _ = "IEND"
    chunkData _ = BL.empty
