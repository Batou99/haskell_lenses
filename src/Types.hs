module Types where

import Data.Map as Map

data Person = P { _name :: String
                , _addr :: Address
                , _salary :: Int }  deriving Show

data Address = A { _road :: String
                 , _city :: String
                 , _postcode :: String } deriving Show

type Point2D = (Float, Float)
type Vector = (Point2D, Point2D)

data Product = Product { _productId :: Int
                       , _productName :: String
                       , _productPrice :: Float } deriving Show

type Catalog = Map Int Product

data Store = Store { _storeName :: String
                   , _storeCatalog :: Catalog } deriving Show
