--Define a linear recursive
--lcVariance :: [Float] -> Float
--that computes the variance (Sn)^2 of a list. Your function must use lcVariance xs to
--compute lcVariance (x:xs). Note, fromIntegral.length is compatible with Float.

lcVariance :: [Float] -> Float
lcVariance [] = 0
lcVariance (x:xs) = (1/fromIntegral(length (x:xs))) * (x^2 + fromIntegral(length (xs)) * avg (x:xs) ^2 )
    where
        avg :: [Float] -> Float
        avg [] = 0
        avg (x:xs) = (x + fromIntegral(length (xs)) * avg (xs)) / fromIntegral(length (x:xs))