module Main (main) where

import Data.Complex

maxIters :: Int
maxIters = 1000

coordToComplex :: Int -> Int -> Complex Double
coordToComplex x y = fromIntegral x :+ fromIntegral y

getIters :: Complex Double -> Int
getIters c = steps
  where
    steps = helper c 0 0
    helper c' z iters
      | iters >= maxIters = iters
      | magnitude newZ > 2 = iters
      | otherwise = helper c' newZ (iters + 1)
      where
        newZ = z * z + c

grayscale :: Int -> Int
grayscale iters = round ((255 / fromIntegral maxIters) * fromIntegral iters)

main :: IO ()
main = print (grayscale(getIters ((-0.75) :+ 0.1)))