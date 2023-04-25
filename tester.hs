
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

