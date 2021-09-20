{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Font.VariationAxis (
	-- * AXIS CLASS
	PangoFontDescriptionAxis,
	-- * ADD AXIS
	pangoFontDescriptionAddAxis,
	-- * SET AND GET VARIATIONS
	Variations,
	-- * Others
	showVariations, readVariations, variationsSetAxis, variationsGetAxis
	) where

import Control.Arrow

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Language.Haskell.TH

class PangoFontDescriptionAxis a where
	pangoFontDescriptionAxisTag :: BS.ByteString
	pangoFontDescriptionAxisToDouble :: a -> Double
	pangoFontDescriptionAxisFromDouble :: Double -> a

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

newtype Variations = Variations { getVariations :: M.Map BS.ByteString Double }
	deriving Show

pangoFontDescriptionAddAxis :: String -> String -> DecsQ
pangoFontDescriptionAddAxis a t = (\n i -> [n, i])
	<$> pangoFontDescriptionAddAxisNewtype a
	<*> pangoFontDescriptionAddAxisInstance a t

pangoFontDescriptionAddAxisNewtype :: String -> DecQ
pangoFontDescriptionAddAxisNewtype a =
	newtypeD (cxt [])
		(mkName a) [] Nothing (recC (mkName a) [
			varBangType (mkName $ "get" ++ a)
				$ bangType (bang noSourceUnpackedness noSourceStrictness) (conT ''Double) ])
		[derivClause Nothing [conT ''Show]]

pangoFontDescriptionAddAxisInstance :: String -> String -> DecQ
pangoFontDescriptionAddAxisInstance a t = instanceD (cxt []) (conT ''PangoFontDescriptionAxis `appT` conT (mkName a)) [
	valD (varP 'pangoFontDescriptionAxisTag) (normalB . litE $ StringL t) [],
	valD (varP 'pangoFontDescriptionAxisToDouble) (normalB . varE . mkName $ "get" ++ a) [],
	valD (varP 'pangoFontDescriptionAxisFromDouble) (normalB . conE $ mkName a) []
	]

showVariations :: Variations -> BS.ByteString
showVariations = BS.intercalate "," . ((\(a, v) -> a <> "=" <> v) . (id *** BSC.pack . show) <$>) . M.toList . getVariations

readVariations :: BS.ByteString -> Variations
readVariations = Variations . M.fromList . ((\[a, v] -> (a, read $ BSC.unpack v)) . BSC.split '=' <$>) . BSC.split ','

variationsSetAxis :: forall a . PangoFontDescriptionAxis a => a -> Variations -> Variations
variationsSetAxis a = Variations . M.insert
		(pangoFontDescriptionAxisTag @a)
		(pangoFontDescriptionAxisToDouble a) . getVariations

variationsGetAxis ::
	forall a . PangoFontDescriptionAxis a => Variations -> Maybe a
variationsGetAxis (Variations as) = pangoFontDescriptionAxisFromDouble
	<$> M.lookup (pangoFontDescriptionAxisTag @a) as
