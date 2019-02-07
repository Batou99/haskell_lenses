{-# LANGUAGE RankNTypes #-}

module Traversals where

import Types
import Maps
import Control.Lens
import Control.Lens.Traversal
import Control.Lens.Fold

-- set traversed 3 [1, 2] # [3, 3]
-- over traversed (*2) [1, 2] # [2, 4]
-- view traversed ["h", "e", "l", "l", "o"] # "hello"
-- getSum $ view traversed $ over traversed Sum [1, 2, 3] -- 6

productsLens :: Traversal' Store Product
productsLens = storeCatalog . traversed

sumPrices :: Store -> Float
sumPrices = foldlOf (storeCatalog . traversed . productPrice) (+) 0

sumPrices' :: Store -> Float
sumPrices' store = sum $ toListOf (storeCatalog . traversed . productPrice) store

negateAll :: [[Int]] -> [[Int]]
negateAll = over (traversed . traversed) negate

addProductWithAt :: Product -> Catalog -> Catalog
addProductWithAt product = set (at $ _productId product) (Just product)

addProductWithElement :: Product -> Catalog -> Catalog
addProductWithElement product = set (element $ _productId product) product

discountProduct :: Store -> Int -> Float -> Store
discountProduct store pid percent = over storeProductPriceLens (* percent) store
  where
    storeProductPriceLens = storeCatalog . at pid . traversed . productPrice
