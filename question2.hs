--import Test.Hspec
import Test.QuickCheck
--Define a linear recursive
--lcVariance :: [Float] -> Float
--that computes the variance (Sn)^2 of a list. Your function must use lcVariance xs to
--compute lcVariance (x:xs). Note, fromIntegral.length is compatible with Float.
--Q2:
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

--Q3:
-- initial case: n = 1, xbar = 0, variance = 0
trVariance :: Float -> Float -> Float -> [Float] -> Float
--trVariance _ _ _ [] = undefined
trVariance _ _ variance [] = variance
trVariance n xbar variance (x:xs) = trVariance (n+1) avgnew varianceNew xs
    where
        avgnew :: Float  
        avgnew = (x+n*xbar)/(n+1)
        varianceNew :: Float
        varianceNew = 
            (1/(n+1)) * (x^2 + n * (variance + xbar ^2)) - avgnew ^2

--Q4:
variance :: [Float] -> Float
variance [] = 0
variance xs = trVariance 0.0 0.0 0.0 xs

--Q5:

--testing area
sum'::Int->Int->Int
sum' a b = a + b