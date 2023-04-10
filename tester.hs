data Lists a = Value a | ConnectTo a (Lists a) deriving (Show)
-- Empty and Connect is a constructor! (first char has to be capital)
-- Have to derive Show to print the list! (deriving is the key word)

xs = Value 5
ys = ConnectTo 5 xs
zs = ConnectTo 6 ys

type Abc = (Int, Char)
-- Type is a constructor! (first char has to be capital)


type A = Int -> Int

