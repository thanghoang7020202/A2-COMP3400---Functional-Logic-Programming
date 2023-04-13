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

-- why this result is population variance??? not sample variance?
--how to find the invariance of the variance?

-- initial case: n = 1, avgfull = 0, variance = 0
trVariance :: Float -> Float -> Float -> [Float] -> Float
trVariance _ _ _ [] = undefined
trVariance _ _ variance [x] = variance 
trVariance n avgfull variance (x:xs) = trVariance (n+1) avgnew ((1/(n+1)) * (x^2 + n * (variance + avgfull ^2)) - avgnew ^2) xs
    where avgnew = (x+n*avgfull)/(n+1)

variance :: [Float] -> Float
variance = trVariance 1 0 0

