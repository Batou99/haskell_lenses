{-# LANGUAGE Rank2Types #-}

module Index where

import Types
import Maps
import Control.Lens
import Control.Lens.At

discountProduct :: Int -> Float -> Catalog -> Catalog
discountProduct pid percent = over (ix pid . productPrice) (*percent)

atIndexPid :: Int -> Traversal' Catalog Product
atIndexPid = ix 

setIndex1 :: Product -> Catalog -> Catalog
setIndex1 product = set (ix $ _productId product) product

