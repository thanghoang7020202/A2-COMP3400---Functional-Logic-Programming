import Data.List (findIndex)
import Control.Applicative ((<|>))

shortest :: [[a]] -> Maybe [a]
shortest [] = Nothing
shortest xss = Just $ pick xss (hShortest xss)

pick :: [[a]] -> [[a]] -> [a]
pick (xs:xss) ([]:yss) = xs
pick (xs:xss) (ys:yss) = pick xss yss

hShortest :: [[a]] -> [[a]]
hShortest xss
    | any null xss = xss
    | otherwise    = hShortest (tail <$> xss)

shortest' :: [[a]] -> Maybe [a]
shortest' [] = Nothing
shortest' lsts = (lsts !!) <$> nullIx lsts
  where
    nullIx :: [[a]] -> Maybe Int
    nullIx lsts = findIndex null lsts <|> nullIx (tail <$> lsts)