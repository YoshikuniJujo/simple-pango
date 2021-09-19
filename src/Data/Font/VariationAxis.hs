{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Font.VariationAxis (
	-- * AXIS CLASS
	PangoFontDescriptionAxis,
	pangoFontDescriptionAxisTag,
	pangoFontDescriptionAxisToDouble, pangoFontDescriptionAxisFromDouble,
	-- * ADD AXIS
	pangoFontDescriptionAddAxis,
	-- * SET AND GET VARIATIONS
	Variations,
	) where

import qualified Data.Map as M
import qualified Data.ByteString as BS

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

type Variations = M.Map BS.ByteString Double

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
