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

newtype FunMap k v
  = FunMap
    { getFunMap :: k -> Maybe v }

empty :: FunMap k v
empty = FunMap (\_ -> Nothing)

lookup :: Eq k => k -> FunMap k v -> Maybe v
lookup k (FunMap hash) = hash k

insert :: Eq k => k -> v -> FunMap k v -> FunMap k v
insert k v hashmap = FunMap hash'
  where
    hash' k'
      | k' == k   = Just $ v
      | otherwise = lookup k' hashmap

delete :: Eq k => k -> FunMap k v -> FunMap k v
delete k hashmap = FunMap hash'
  where
    hash' k'
      | k' == k   = Nothing
      | otherwise = lookup k' hashmap


-- YOU COULD ALSO DO THIS WITH A FOLD
fromList :: Eq k => [(k,v)] -> FunMap k v
fromList = fromList'.reverse

fromList' :: Eq k => [(k,v)] -> FunMap k v
fromList' [] = empty
fromList' ((k, v):xs) = insert k v $ fromList' xs

-- DO IT WITH A FOLD...
-- fromList = foldl (\hash (k, v) -> insert k v hasg) empty

instance Functor (FunMap a) where
    -- fmap :: (b -> c) -> (FunMap a) b -> (FunMap a) c
    fmap h (FunMap g) = FunMap $ (\x -> h <$> g x)

changeCurrency :: (Int -> Int) -> FunMap String Int -> FunMap String Int
changeCurrency = fmap

convertMarks :: FunMap String Int -> FunMap String Char
convertMarks hash = foo <$> hash
  where
    foo x
        | x >= 84 = 'A'
        | x >= 67 = 'B'
        | x >= 50 = 'C'
        | x >= 34 = 'D'
        | x >= 17 = 'E'
        | otherwise = 'F'
