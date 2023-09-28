module Blockus (tile) where

type Board = [[Int]]

tile :: Int -> Board
tile k
    | k < 0     = [[]]
    | otherwise = hTile k [1..numPieces] where numPieces = div ((2^k)^2-1) 3

hTile :: Int -> [Int] -> Board
hTile 0 _ = [[0]]
hTile k (x:xs) = (zipWith (++) nw ne) ++ (zipWith (++) sw se) -- combine the boards
    where
        [as, bs, cs, ds] = concat.map halves.halves $ xs      -- break xs into four pieces of equal length
        nw = hTile (k-1) as                                   -- recurse to get NW board
        ne = reverse $ replaceCorner x $ hTile (k-1) bs       -- recurse to get NE board and do vertical flip
        sw = map reverse $ replaceCorner x $ hTile (k-1) cs   -- recurse to get SW board and do horizontal flip
        se = replaceCorner x $ hTile (k-1) ds                 -- recurse to get SE board

replaceCorner :: Int -> Board -> Board
replaceCorner k ((x:xs):xss) = (k:xs):xss -- replace the northwest corner of board

halves :: [a] -> [[a]]
halves xs = toList $ splitAt (quot (length xs) 2) xs
    where
        toList (x,y) = [x,y]