module Shortest (shortest) where
import Language.Haskell.TH.Syntax (counter)

-- Do not modify anything above this line.
--
-- This question is worth 10 POINTS

{--
shortest is a function that receives a list of lists and returns the shortest 
list if there is one, and Nothing otherwise.

	shortest :: [[a]] -> Maybe [a]

Your implementation must satisfy the following requirements:
1. It must work even if there are infinite lists in the input (see *).
2. It must work even if the input list itself is infinite (see *).
3. If there are several lists with the minimal length, return the first of them.

	* We guarantee that for all test inputs (visible and hidden), it will be 
possible to find a solution within a reasonable amount of time using an 
appropriate algorithm. For example, there won’t be any infinite lists of 
infinite lists in the tests.

For example,
        shortest  [[1, 3..], [10..], repeat 5, [2, 4], [2, 4..], [42, 228]] 
            == Just [2, 4]
--}

shortest :: Eq a => [[a]] -> Maybe [a]
shortest [] = Nothing
shortest (x:xs) 
    | null x = Just []
    | otherwise = shortest' 0 (x:xs) (x:xs)
    where
        cutter:: Int -> [[a]] -> [[a]] -> Maybe [a]
        cutter k originalList [] = Nothing
        cutter k originalList (x:xs) = shortest' k originalList [drop k y | y <- originalList ] 
        shortest':: Int -> [[a]] -> [[a]] -> Maybe [a]
        shortest' k originalList [] = Nothing
        shortest' k originalList (x:xs) 
            | ifNotInfinity 0 (x:xs) == -1 = cutter (k+1) originalList (x:xs)
            | otherwise = Just (originalList !! ifNotInfinity 0 (x:xs))
        ifNotInfinity:: Int -> [[a]] -> Int
        ifNotInfinity index [] = -1
        ifNotInfinity index (x:xs)
            | null x = index
            | otherwise = ifNotInfinity (index + 1) xs



