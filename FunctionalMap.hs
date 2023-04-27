module FunctionalMap
  ( FunMap (..)
  , empty
  , lookup
  , insert
  , delete
  , fromList
  , changeCurrency
  , convertMarks
  ) where

import Prelude hiding (lookup)
-- Do not modify anything above this line.
--
-- This question is worth 20 POINTS

{--
In this task, you will have to implement a map (dictionary, key-(getFunMap map key) storage) which will hold keys of type k and (getFunMap map key)s of type v.

The data type for this map is
--}

newtype FunMap k v
  = FunMap
    { getFunMap :: k -> Maybe v }
-- define getter to that type
--
-- Part 1.
--
-- Your task is to implement the usual operations for maps:
-- A function which returns an empty map (a map with no keys and (getFunMap map key)s):

empty :: FunMap k v
empty = FunMap (\_ -> Nothing)

-- A function to search elements in the map by key.
-- If there is an element associated with the given key, the function should return Just element.
-- Otherwise it should return Nothing.

lookup :: Eq k => k -> FunMap k v -> Maybe v
lookup key map = getFunMap map key
  

-- A function to insert an element into the map and associate it with the given key.
-- If there already IS an element associated with that key, overwrite it.

insert :: Eq k => k -> v -> FunMap k v -> FunMap k v
insert k v map = FunMap(\key -> if key == k then Just v else lookup k map)

-- A function to remove the element and the associated key from the map.
-- If there was no element associated with the key, return the map unchanged.

delete :: Eq k => k -> FunMap k v -> FunMap k v
delete k map = FunMap (\key -> if key == k then Nothing else lookup k map) 

-- A function to construct a map from a list of key-(getFunMap map key) pairs.
-- If there are several (getFunMap map key)s associated with the same key in the list,
-- the last one that appears in the list should be stored in the map.

fromList :: Eq k => [(k,v)] -> FunMap k v
fromList [] = empty
fromList (x:xs) = FunMap(\key -> if key == fst x then Just (snd x) else lookup key (fromList xs))

-- Part 2.
--
-- Implement the Functor instance for this map:

instance Functor (FunMap k) where
  --fmap :: (a->b) -> FunMap k a -> FunMap k b
    fmap f map = FunMap(\key -> fmap f (getFunMap map key)) 

     

-- fmap should only change elements ((getFunMap map key)s) in the map applying the provided function to them.
-- It should not change keys nor the structure of the map.

-- Using the Functor instance you just defined, implement the following functions:
-- The history of bank transactions is stored in FunMap String Int
-- where String is the name of the transaction and Int is the amount of money in some currency.
-- You need to be able to convert all transactions into another currency given the exchange rate.
-- Implement function

changeCurrency :: (Int -> Int) -> FunMap String Int -> FunMap String Int
changeCurrency = fmap

-- Students’ marks are stored in the grading system in FunMap String Int where
-- String represents student’s name and Int represents their mark in the range [0, 100].
-- You need to write a function that will transform all the marks from the [0, 100] range
-- to ‘A’ to ‘F’ range using the following agreements:
--
-- 84 <= mark <= 100 -> ‘A’
-- 67 <= mark <= 83 -> ‘B’
-- 50 <= mark <= 66 -> ‘C’
-- 34 <= mark <= 49 -> ‘D’
-- 17 <= mark <= 33 -> ‘E’
-- 0 <= mark <= 16 -> ‘F’

convertMarks :: FunMap String Int -> FunMap String Char
convertMarks = fmap converter
  where 
    converter mark
      | mark >= 84 && mark <= 100 = 'A'
      | mark >= 67 && mark <= 83 = 'B'
      | mark >= 50 && mark <= 66 = 'C'
      | mark >= 34 && mark <= 49 = 'D'
      | mark >= 17 && mark <= 33 = 'E'
      | otherwise = 'F'

{-convertMarks :: FunMap String Int -> FunMap String Char
convertMarks = FunMap(\key -> if within (getFunMap map key) 84 100 then Just 'A' else 
    if within (getFunMap map key) 67 83 then Just 'B' else
    if within (getFunMap map key) 50 66 then Just 'C' else
    if within (getFunMap map key) 34 49 then Just 'D' else
    if within (getFunMap map key) 17 33 then Just 'E' else Just 'F')
      where within val a b = lne a val && gne b val
            gne :: Int -> Maybe Int -> Bool 
            gne a (Just b) = a >= b
            gne _ Nothing = False
            lne :: Int -> Maybe Int -> Bool 
            lne a (Just b) = a <= b
            lne _ Nothing = False-}