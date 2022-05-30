module PNG.Constants (
    BitDepth (..),
    ColorType (..),
    CompressionMethod (..),
    FilterMethod (..),
    InterlaceMethod (..),
    toBitDepth,
    fromBitDepth,
    toColorType,
    fromColorType,
    toCompressionMethod,
    fromCompressionMethod,
    toFilterMethod,
    fromFilterMethod,
    toInterlaceMethod,
    fromInterlaceMethod
) where

import qualified Data.Word as DW

data BitDepth = OneBit | TwoBit | FourBit | EightBit | SixteenBit
    deriving (Eq, Show)
data ColorType = Grayscale | TrueColor | Indexed | GrayscaleAlpha | TrueColorAlpha
    deriving (Eq, Show)
data CompressionMethod = Deflate
    deriving (Eq, Show)
data FilterMethod = NoFilter | Sub | Up | Average | Paeth
    deriving (Eq, Show)
data InterlaceMethod = NoInterlace | Adam7
    deriving (Eq, Show)

toBitDepth :: DW.Word8 -> BitDepth
toBitDepth depth = case depth of
    1 -> OneBit
    2 -> TwoBit
    4 -> FourBit
    8 -> EightBit
    16 -> SixteenBit

fromBitDepth :: BitDepth -> DW.Word8
fromBitDepth depth = case depth of
    OneBit -> 1
    TwoBit -> 2
    FourBit -> 4
    EightBit -> 8
    SixteenBit -> 16

toColorType :: DW.Word8 -> ColorType
toColorType color = case color of
    0 -> Grayscale
    2 -> TrueColor
    3 -> Indexed
    4 -> GrayscaleAlpha
    6 -> TrueColorAlpha

fromColorType :: ColorType -> DW.Word8
fromColorType color = case color of
    Grayscale -> 0
    TrueColor -> 2
    Indexed -> 3
    GrayscaleAlpha -> 4
    TrueColorAlpha -> 6

toCompressionMethod :: DW.Word8 -> CompressionMethod
toCompressionMethod method = case method of
    0 -> Deflate

fromCompressionMethod :: CompressionMethod -> DW.Word8
fromCompressionMethod method = case method of
    Deflate -> 0

toFilterMethod :: DW.Word8 -> FilterMethod
toFilterMethod method = case method of
    0 -> NoFilter
    1 -> Sub
    2 -> Up
    3 -> Average
    4 -> Paeth

fromFilterMethod :: FilterMethod -> DW.Word8
fromFilterMethod method = case method of
    NoFilter -> 0
    Sub -> 1
    Up -> 2
    Average -> 3
    Paeth -> 4

toInterlaceMethod :: DW.Word8 -> InterlaceMethod
toInterlaceMethod method = case method of
    0 -> NoInterlace
    1 -> Adam7

fromInterlaceMethod :: InterlaceMethod -> DW.Word8
fromInterlaceMethod method = case method of
    NoInterlace -> 0
    Adam7 -> 1
