module Basic where

import Control.Lens

type Point2D = (Float, Float)

origin :: Point2D
origin = (0, 0)

viewX :: Point2D -> Float
viewX (x, _) = x

viewY :: Point2D -> Float
viewY (_, y) = y

setX :: Float -> Point2D -> Point2D
setX newX (_, y) = (newX, y)

setY :: Float -> Point2D -> Point2D
setY newY (x, _) = (x, newY)

overX :: (Float -> Float) -> Point2D -> Point2D
overX fn (x, y) = (fn x, y)

overY :: (Float -> Float) -> Point2D -> Point2D
overY fn (x, y) = (x,fn y)

-- e.g overY (+1) (1, 10) == (1, 11)

viewX' = view _1 origin -- 1

setX' = set _1 2 origin -- (2, 0)

overX' = over _1 (+10) origin -- (11, 0)

-- Lens laws

setGet = view _1 $ set _1 10 origin -- 10

getSet = set _1 (view _1 origin) origin -- No changes


-- Idempotent
setSet = set _1 4 $ set _1 4 origin

