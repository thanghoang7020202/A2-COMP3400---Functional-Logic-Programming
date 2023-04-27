
data Lists a = Value a | ConnectTo a (Lists a) deriving (Show)
-- Empty and Connect is a constructor! (first char has to be capital)
-- Have to derive Show to print the list! (deriving is the key word)

xs :: Lists Integer
xs = Value 5
ys = ConnectTo 5 xs
zs = ConnectTo 6 ys

type Abc = (Int, Char)
-- Type is a constructor! (first char has to be capital)


type A = Int -> Int

lavg :: [Float] -> Float
lavg [] = 0
lavg (x:xs) = (x + n * lavg xs) / (n+1)
    where
        n = fromIntegral(length xs)

--initial case: n = 0, avg = 0
tavg :: Float -> Float -> [Float] -> Float
tavg _ avg [] = avg
tavg n avg (x:xs) = tavg (n+1) ((x+n*avg)/(n+1)) xs

--fmap :: (a->b) -> f a -> f b
fmap' f (Value x) = Value (f x)
fmap' f (ConnectTo x xs) = ConnectTo (f x) $ fmap' f xs

--fmap :: (a->b) -> f a -> f b
fmap'' :: (a-> b) -> Maybe a -> Maybe b
fmap'' f (Just x) = Just (f x)
fmap'' f Nothing = Nothing

--instance Functor []  where
--    fmap f [] = []:[1]
--    fmap f (x:xs) = f x : [1] : fmap f xs
-- k = 3 -> 2^3 * 2^3 = 64 - 1 = 63/ 3 = 21 -1 = 20 /4 = 5 -1 = 1 
tile :: Int -> [[Int]]
tile 0 = [[0]]
tile k
        | k < 0 = []
        | otherwise = tile' k [1 .. ((negate 1 + (2^k * 2^k)) `div` 3)]

tile' ::Int -> [Int] -> [[Int]]
tile' 1 [x] = [[0,x],[x,x]]
tile' k xs = zipWith (++)  (tile' (k-1) (qua1 k xs xs)) (zeroupdater xs (reverse $ tile' (k-1) (qua2 k xs)) ) 
        ++
        zipWith (++) (zeroupdater xs (map reverse $ tile' (k-1) (qua3 k xs))) (zeroupdater xs (tile' (k-1) (qua4 k xs)))
qua1 k xs= take (quarter k xs)
qua2 k xs = take (quarter k xs) (drop (quarter k xs) xs)
qua3 k xs = take (quarter k xs) (drop (2*quarter k xs) xs)
qua4 k xs = take (quarter k xs) (drop (3*quarter k xs) xs)

quarter k xs = length xs `div` 4

zeroupdater :: (Eq b, Num b) => [b] -> [[b]] -> [[b]]
zeroupdater xs = map (map (\i -> if i == 0 then last xs else i))
