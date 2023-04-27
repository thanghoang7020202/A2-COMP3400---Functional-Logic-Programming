module Blockus (tile) where

-- Do not modify anything above this line.
--
-- This question is worth 10 POINTS

{--
Implement the tiling from Q10 in the Written Assignment.

Implement function 
	tile :: Int -> [[Int]]

which will construct the tiling of a board 2^k x 2^k for the given k.
If k is less than zero, return an empty list.
In the output, each V3 piece should be assigned a unique positive non-zero number. 
The upper left corner should be assigned a 0. 
For example, the tiling for a 4x4 Board might look like this:

0 1 4 4 
1 1 2 4
5 2 2 3
5 5 3 3

So, 
    tile 2 == [[0, 1, 4, 4]
              ,[1, 1, 2, 4] 
              ,[5, 2, 2, 3] 
              ,[5, 5, 3, 3]]

Note that this solution is not unique. Any valid solution will be accepted.

--}

--number of blocks to fill = (2^k * 2^k -1) /3
tile :: Int -> [[Int]]
tile 0 = [[0]]
tile k
        | k < 0 = []
        | otherwise = tile' k [1 .. ((negate 1 + (2^k * 2^k)) `div` 3)]
        where
                tile' ::Int -> [Int] -> [[Int]]
                tile' 1 [x] = [[0,x],[x,x]]
                tile' k xs = zipWith (++)  (tile' (k-1) (qua1 k xs)) (zeroupdater xs (reverse $ tile' (k-1) (qua2 k xs)) ) 
                        ++
                        zipWith (++) (zeroupdater xs (map reverse $ tile' (k-1) (qua3 k xs))) (zeroupdater xs (tile' (k-1) (qua4 k xs)))
                qua1 :: Int -> [a] -> [a]
                qua1 k xs = take (quarter k xs) xs
                qua2 :: Int -> [a] -> [a]
                qua2 k xs = take (quarter k xs) (drop (quarter k xs) xs)
                qua3 :: Int -> [a] -> [a]
                qua3 k xs = take (quarter k xs) (drop (2*quarter k xs) xs)
                qua4 :: Int -> [a] -> [a]
                qua4 k xs = take (quarter k xs) (drop (3*quarter k xs) xs)
                quarter :: Int -> [a] -> Int
                quarter k xs = length xs `div` 4
                zeroupdater :: (Eq b, Num b) => [b] -> [[b]] -> [[b]]
                zeroupdater xs = map (map (\i -> if i == 0 then last xs else i))