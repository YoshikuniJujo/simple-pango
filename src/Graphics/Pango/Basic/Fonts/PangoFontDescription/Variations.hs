{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations (
	-- * SET AND GET AXIS
	pangoFontDescriptionSetAxis, pangoFontDescriptionGetAxis,
	-- * SET AND GET VARIATIONS
	Variations,
	pangoFontDescriptionSetVariationsMap,
	pangoFontDescriptionGetVariationsMap,
	-- * DEFAULT AXES
	Weight(..), Width(..), Slant(..), Italic(..), OpticalSize(..)
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Control.Arrow
import Control.Monad.Primitive

import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import System.IO.Unsafe

import Data.Font.VariationAxis

pangoFontDescriptionSetAxis :: forall a m .
	(PangoFontDescriptionAxis a, PrimMonad m) =>
	PangoFontDescriptionPrim (PrimState m) -> a -> m ()
pangoFontDescriptionSetAxis fd a = do
	as <- pangoFontDescriptionGetVariationsMap fd
	pangoFontDescriptionSetVariationsMap fd $ M.insert
		(pangoFontDescriptionAxisTag @a)
		(pangoFontDescriptionAxisToDouble a) as

pangoFontDescriptionGetAxis ::
	forall a . PangoFontDescriptionAxis a => PangoFontDescription -> Maybe a
pangoFontDescriptionGetAxis fd = unsafePerformIO do
	pangoFontDescriptionThaw fd >>= \case
		fd' -> do
			as <- pangoFontDescriptionGetVariationsMap fd'
			pure $ pangoFontDescriptionAxisFromDouble
				<$> M.lookup (pangoFontDescriptionAxisTag @a) as

newtype Weight = Weight { getWeight :: Double } deriving Show

instance PangoFontDescriptionAxis Weight where
	pangoFontDescriptionAxisTag = "wght"
	pangoFontDescriptionAxisToDouble = getWeight
	pangoFontDescriptionAxisFromDouble = Weight

newtype Width = Width { getWidth :: Double } deriving Show

instance PangoFontDescriptionAxis Width where
	pangoFontDescriptionAxisTag = "wdth"
	pangoFontDescriptionAxisToDouble = getWidth
	pangoFontDescriptionAxisFromDouble = Width

newtype Italic = Italic { getItalic :: Double } deriving Show

instance PangoFontDescriptionAxis Italic where
	pangoFontDescriptionAxisTag = "ital"
	pangoFontDescriptionAxisToDouble = getItalic
	pangoFontDescriptionAxisFromDouble = Italic

newtype OpticalSize = OpticalSize { getOpticalSize :: Double } deriving Show

instance PangoFontDescriptionAxis OpticalSize where
	pangoFontDescriptionAxisTag = "opsz"
	pangoFontDescriptionAxisToDouble = getOpticalSize
	pangoFontDescriptionAxisFromDouble = OpticalSize

newtype Slant = Slant { getSlant :: Double } deriving Show

instance PangoFontDescriptionAxis Slant where
	pangoFontDescriptionAxisTag = "slnt"
	pangoFontDescriptionAxisToDouble = getSlant
	pangoFontDescriptionAxisFromDouble = Slant

showVariations :: Variations -> BS.ByteString
showVariations = BS.intercalate "," . ((\(a, v) -> a <> "=" <> v) . (id *** BSC.pack . show) <$>) . M.toList

readVariations :: BS.ByteString -> Variations
readVariations = M.fromList . ((\[a, v] -> (a, read $ BSC.unpack v)) . BSC.split '=' <$>) . BSC.split ','

pangoFontDescriptionSetVariationsMap :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> Variations -> m ()
pangoFontDescriptionSetVariationsMap (PangoFontDescriptionPrim ffd) v = unsafeIOToPrim
	$ withForeignPtr ffd \pfd -> BS.useAsCString (showVariations v) \cv ->
		c_pango_font_description_set_variations pfd cv

foreign import ccall "pango_font_description_set_variations"
	c_pango_font_description_set_variations ::
	Ptr PangoFontDescription -> CString -> IO ()

pangoFontDescriptionGetVariationsMap :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> m Variations
pangoFontDescriptionGetVariationsMap (PangoFontDescriptionPrim ffd) = unsafeIOToPrim
	$ withForeignPtr ffd \pfd -> readVariations <$>
		(myPackCString =<< c_pango_font_description_get_variations pfd)

myPackCString :: CString -> IO BS.ByteString
myPackCString cs | cs == nullPtr = pure "" | otherwise = BS.packCString cs

foreign import ccall "pango_font_description_get_variations"
	c_pango_font_description_get_variations ::
	Ptr PangoFontDescription -> IO CString
