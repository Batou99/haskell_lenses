{-# LANGUAGE Rank2Types #-}

module Prisms where

import Types
import Color
import Control.Lens
import Control.Lens.Prism
import Control.Lens.Fold
import Control.Lens.Review

newtype Percent = Percent Float deriving (Eq, Show)
data Point      = Point Float Float deriving (Eq, Show)

data Fill
  = Solid Color
  | LinearGradient Color Color Percent
  | RadialGradient Color Color Point
  | NoFill deriving (Eq, Show)

fillBlackToWhite :: Fill
fillBlackToWhite = LinearGradient Color.black Color.white $ Percent 3.3

fillWhiteToBlack :: Fill
fillWhiteToBlack = LinearGradient Color.white Color.black $ Percent 3.3

fillRadial :: Fill
fillRadial = RadialGradient Color.white Color.black $ Point 1.0 3.4

getColor :: Fill -> Maybe Color
getColor = preview _solidFill 

getWhite = getColor $ Solid Color.white -- Just Color 255 255 255 0
getNothing = getColor NoFill -- Nothing

getSolidWhite :: Fill
getSolidWhite = review _solidFill Color.white

tuple = ("foo", Solid Color.white)

setColorOnColor :: Fill
setColorOnColor = set _solidFill Color.white $ Solid Color.black

setColorOnTuple :: (String, Fill)
setColorOnTuple = set (_2 . _solidFill) Color.black tuple



_solidFill :: Prism' Fill Color
_solidFill = prism' constructor focuser
  where
    constructor = Solid
    focuser fill = case fill of
      Solid color -> Just color
      otherCases -> Nothing

_solidWhite :: Prism' Fill ()
_solidWhite = only (Solid Color.white)

_solidWhite' :: Prism' Fill ()
_solidWhite' = nearly (Solid Color.white) eqFunction
  where
    eqFunction x = case x of
                     Solid color -> color == Color.white
                     _           -> False

reviewPreview :: Maybe Color
reviewPreview = preview _solidFill solidColorWhite
  where
    solidColorWhite = review _solidFill Color.white

previewReview :: Maybe Fill
previewReview = review _solidFill <$> maybeColor
  where
    maybeColor = preview _solidFill $ Solid Color.white


_centerPoint :: Prism' Fill Point
_centerPoint = prism' constructor focuser
  where
    focuser x = case x of
                  RadialGradient _ _ point -> Just point
                  _ -> Nothing
    constructor = RadialGradient Color.black Color.white

data RadialInterchange = RadialInterchange Color Color Point

centerPoint' :: Prism' Fill RadialInterchange
centerPoint' = prism' constructor focuser
  where
    constructor (RadialInterchange color1 color2 center) = RadialGradient color1 color2 center
    focuser x = case x of
                  RadialGradient color1 color2 center -> Just $ RadialInterchange color1 color2 center
                  _ -> Nothing
