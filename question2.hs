--Define a linear recursive
--lcVariance :: [Float] -> Float
--that computes the variance (Sn)^2 of a list. Your function must use lcVariance xs to
--compute lcVariance (x:xs). Note, fromIntegral.length is compatible with Float.

lcVariance :: [Float] -> Float
lcVariance [] = undefined
lcVariance [x] = 0
lcVariance (x:xs) = (1/nplus1) * (x^2 + n * (lcVariance xs + avg xs ^2)) - avg (x:xs) ^2
    where
        avg :: [Float] -> Float
        avg ys = sum ys / fromIntegral(length ys)
        nplus1 :: Float
        nplus1 = fromIntegral(length (x:xs))
        n :: Float
        n = fromIntegral(length xs)


trVariance :: Float -> Float -> Float -> [Float] -> Float
trVariance _ _ _ [] = undefined
trVariance _ _ _ [x] = 0
trVariance nplus1 n avg (x:xs) = undefined