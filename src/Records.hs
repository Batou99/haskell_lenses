{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE RankNTypes #-}

module Records where

import Types
import Control.Lens

person :: Person
person = P { _name = "Lorenzo"
           , _addr = address
           , _salary = 100000 }


address :: Address
address = A { _road = "Calle Eloy Gonzalo 23"
            , _city = "Madrid"
            , _postcode = "2810" }


-- First level records

viewName :: Person -> String
viewName = _name

setName :: String -> Person -> Person
setName name person = person { _name = name }

overName :: (String -> String) -> Person -> Person
overName fn person = person { _name = fn $ _name person }

-- Nested records

viewPersonRoad :: Person -> String
viewPersonRoad = _road . _addr

setPersonRoad :: String -> Person -> Person
setPersonRoad newRoad person = person { _addr = newAddr }
  where
    oldAddr = _addr person
    newAddr = oldAddr { _road = newRoad }

overPersonRoad :: (String -> String) -> Person -> Person
overPersonRoad fn person = setPersonRoad (fn oldPersonRoad) person
  where
    oldPersonRoad = viewPersonRoad person

-- This gets old easily
-- Let's try the same with lenses

salaryLens :: Lens' Person Int
salaryLens = lens _salary (\p s -> p { _salary = s })

addrLens :: Lens' Person Address
addrLens = lens _addr (\p a -> p { _addr = a})

roadLens :: Lens' Address String
roadLens = lens _road (\a r -> a { _road = r })

personRoadLens :: Lens' Person String
personRoadLens = addrLens . roadLens

-- lets try first level records

viewSalary :: Person -> Int
viewSalary = view salaryLens

setSalary :: Int -> Person -> Person
setSalary = set salaryLens

overSalary :: (Int -> Int) -> Person -> Person
overSalary = over salaryLens

viewRoad :: Person -> String
viewRoad = view $ addrLens . roadLens

-- How about with nested records?

viewPersonRoad' :: Person -> String
viewPersonRoad' = view $ addrLens . roadLens

setPersonRoad' :: String -> Person -> Person
setPersonRoad' = set $ addrLens.roadLens

overPersonRoad' :: (String -> String) -> Person -> Person
overPersonRoad' = over $ addrLens.roadLens


-- makeLenses uses Template haskell to build 1 lens per record field
makeLenses ''Person
makeLenses ''Address

viewSalary' :: Person -> Int
viewSalary' = view salary

setSalary' :: Int -> Person -> Person
setSalary' = set salary

overSalary' :: (Int -> Int) -> Person -> Person
overSalary' = over salary

-- How about with nested records?

viewPersonCity :: Person -> String
viewPersonCity = view $ addr . city

setPersonCity :: String -> Person -> Person
setPersonCity = set $ addr.city

overPersonCity :: (String -> String) -> Person -> Person
overPersonCity = over $ addr.city


data LensR s a = LensR {
  viewR :: s -> a,
  setR  :: a -> s -> s,
  overR :: (a -> a) -> s -> s
}

data LensREx s a = LensREx {
  viewREx :: s -> a,
  setREx  :: a -> s -> s,
  overREx :: (a -> a) -> s -> s,
  overRM  :: (a -> Maybe a) -> s -> Maybe s,
  overRIO :: (a -> IO a) -> s -> IO s
}

data LensRSim s a = LensRSim {
  viewRSim :: s -> a,
  setRSim  :: a -> s -> s,
  overRSim :: (a -> a) -> s -> s,
  overRF   :: forall f. Functor f => (a -> f a) -> s -> f s
}

-- and if f is the identity functor then
data LensRSim2 s a = LensRSim2 {
  viewRSim2 :: s -> a,
  setRSim2  :: a -> s -> s,
  overRF2   :: forall f. Functor f => (a -> f a) -> s -> f s
}

-- type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

newtype Id a = Id a
-- Id :: a -> Ida

runId:: Id s -> s
runId (Id x) = x

instance Functor Id where
  fmap f = Id . f . runId

set' :: Lens' s a -> (a -> s -> s)
set' ln x = runId . ln (\_ -> Id x)

type LGetter = forall s a. s -> a
type LSetter = forall s t a. s -> a -> t
type LSetter' =  forall s a. s -> a -> s


lens' :: Functor f => LGetter -> LSetter' -> (a -> f a) -> s -> f s
lens' getter setter = undefined

-- view' :: Functor f => (s -> a) -> (s -> a -> s) -> (a -> f a) -> s -> f s
-- view' :: Lens' s a -> s -> a
-- view' ln s = 
