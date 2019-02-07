module Color where

data Color = Color Int Int Int Float deriving (Eq, Show)

white :: Color
white = Color 255 255 255 1.0

black :: Color
black = Color 0 0 0 1.0

red :: Color
red = Color 255 0 0 1.0

green :: Color
green = Color 0 255 0 1.0

blue :: Color
blue = Color 0 0 255 1.0



