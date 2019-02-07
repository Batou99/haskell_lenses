{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-} 

module Maps where

import Types
import Control.Lens
import Control.Lens.At
import Data.Map as Map hiding (foldl)

makeLenses ''Product
makeLenses ''Store

iPod   = Product { _productId = 1, _productPrice = 232,  _productName = "iPod Touch, 32 GB" }
iPad   = Product { _productId = 2, _productPrice = 729,  _productName = "iPad Pro,   10.5'',  64GB, WiFi" }
iPhone = Product { _productId = 3, _productPrice = 1159, _productName = "iPhone Xs,  64GB" }

products = [iPod, iPad, iPhone]

addToCatalog :: Catalog -> Product -> Catalog
addToCatalog catalog product = Map.insert (product ^. productId) product catalog

catalog :: Catalog
catalog = foldl addToCatalog Map.empty products

store :: Store
store = Store { _storeName = "Apple Store Sol", _storeCatalog = catalog }

-- Lets start with a manual lens

product1Lens :: Lens' Catalog (Maybe Product)
product1Lens = lens getter setter
  where
    getter = Map.lookup 1
    setter catalog wrapped = 
      case wrapped of
        Just prod -> Map.insert 1 prod catalog
        Nothing   -> Map.delete 1 catalog

atKeyLens :: Int -> Lens' Catalog (Maybe Product)
atKeyLens key = lens getter setter
  where
    getter = Map.lookup key
    setter catalog wrapped = 
      case wrapped of
        Just prod -> Map.insert key prod catalog
        Nothing   -> Map.delete key catalog

atKeyLens' :: Int -> Lens' Catalog (Maybe Product)
atKeyLens' = at

storeProduct1 :: Lens' Store (Maybe Product)
storeProduct1 = storeCatalog . at 1

storeProduct :: Int -> Lens' Store (Maybe Product)
storeProduct n = storeCatalog . at n
