module Tuples where

import Types
import Control.Lens

origin = (1, 2)
dest   = (10, 12)
vector = (origin, dest)
-- manual lens

xLens :: Lens' Point2D Float
xLens = lens getter setter
  where
    getter = fst
    setter (_, y) newX = (newX, y)

yLens :: Lens' Point2D Float
yLens = _2

yGeneralLens :: Lens' (a, b) b
yGeneralLens = _2

yEvenMoreGeneralLens :: Lens (ignored, a) (ignored, b) a b
yEvenMoreGeneralLens = _2

-- nested tuples
xOriginLens :: Lens' Vector Float
xOriginLens = _1 . _1

yDestLens :: Lens' Vector Float
yDestLens = _2 . _2

moveOriginX :: (Float -> Float) -> Vector -> Vector
moveOriginX = over (_1 . _1)

-- moveOriginX (+1) vector
