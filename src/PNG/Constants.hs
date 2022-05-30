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

-- Data types

-- The number of bits per sample or per palette index.
-- For example, an eight-bit bit depth allows a granularity between 0 and 255.
data BitDepth = OneBit | TwoBit | FourBit | EightBit | SixteenBit
    deriving (Eq, Show)

-- In essence, the number of channels.
data ColorType = Grayscale | TrueColor | Indexed | GrayscaleAlpha | TrueColorAlpha
    deriving (Eq, Show)

-- The compression method used by the image file.
-- Currently, only Deflate exists in the PNG specification.
data CompressionMethod = Deflate
    deriving (Eq, Show)

-- The pre-filter method used by the image file.
-- Used to achieve superior compression.
data FilterMethod = NoFilter | Sub | Up | Average | Paeth
    deriving (Eq, Show)

-- The interlacing method used by the image file.
-- A bygone artifact from the GIF days, Adam7 is rarely used.
data InterlaceMethod = NoInterlace | Adam7
    deriving (Eq, Show)

-- Functions

-- Converts an 8-bit integer (Word8) into a BitDepth value.
toBitDepth :: DW.Word8 -> BitDepth
toBitDepth depth = case depth of
    1 -> OneBit
    2 -> TwoBit
    4 -> FourBit
    8 -> EightBit
    16 -> SixteenBit

-- Converts a BitDepth value into an 8-bit integer.
fromBitDepth :: BitDepth -> DW.Word8
fromBitDepth depth = case depth of
    OneBit -> 1
    TwoBit -> 2
    FourBit -> 4
    EightBit -> 8
    SixteenBit -> 16

-- Converts an 8-bit integer (Word8) into a ColorType value.
toColorType :: DW.Word8 -> ColorType
toColorType color = case color of
    0 -> Grayscale
    2 -> TrueColor
    3 -> Indexed
    4 -> GrayscaleAlpha
    6 -> TrueColorAlpha

-- Converts a ColorType value into an 8-bit integer.
fromColorType :: ColorType -> DW.Word8
fromColorType color = case color of
    Grayscale -> 0
    TrueColor -> 2
    Indexed -> 3
    GrayscaleAlpha -> 4
    TrueColorAlpha -> 6

-- Converts an 8-bit integer (Word8) into a CompressionMethod value.
toCompressionMethod :: DW.Word8 -> CompressionMethod
toCompressionMethod method = case method of
    0 -> Deflate

-- Converts a CompressionMethod value into an 8-bit integer.
fromCompressionMethod :: CompressionMethod -> DW.Word8
fromCompressionMethod method = case method of
    Deflate -> 0

-- Converts an 8-bit integer (Word8) into a FilterMethod value.
toFilterMethod :: DW.Word8 -> FilterMethod
toFilterMethod method = case method of
    0 -> NoFilter
    1 -> Sub
    2 -> Up
    3 -> Average
    4 -> Paeth

-- Converts a FilterMethod value into an 8-bit integer.
fromFilterMethod :: FilterMethod -> DW.Word8
fromFilterMethod method = case method of
    NoFilter -> 0
    Sub -> 1
    Up -> 2
    Average -> 3
    Paeth -> 4

-- Converts an 8-bit integer (Word8) into an InterlaceMethod value.
toInterlaceMethod :: DW.Word8 -> InterlaceMethod
toInterlaceMethod method = case method of
    0 -> NoInterlace
    1 -> Adam7

-- Converts an InterlaceMethod value into an 8-bit integer.
fromInterlaceMethod :: InterlaceMethod -> DW.Word8
fromInterlaceMethod method = case method of
    NoInterlace -> 0
    Adam7 -> 1
