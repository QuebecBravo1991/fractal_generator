module Main (main) where

import Data.Complex

coordToComplex :: Int -> Int -> Complex Int
coordToComplex x y = x :+ y

main :: IO ()
main = print (coordToComplex 2 3)