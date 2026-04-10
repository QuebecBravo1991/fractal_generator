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

main :: IO ()
main = print (getIters (coordToComplex 0 0))