module Main (main) where

import Codec.Picture
import Data.Complex

outputWidth :: Int
outputWidth = 800 * 3

outputHeight :: Int
outputHeight = 600 * 3

windowStartX :: Double
windowStartX = -2.5

windowEndX :: Double
windowEndX = 1.0

windowStartY :: Double
windowStartY = -1.2

windowEndY :: Double
windowEndY = 1.2

maxIters :: Int
maxIters = 1000

coordToComplex :: Int -> Int -> Complex Double
coordToComplex x y = result
  where
    result = x' :+ y'
    x' = windowStartX + (fromIntegral x / fromIntegral outputWidth) * (windowEndX - windowStartX)
    y' = windowStartY + (fromIntegral y / fromIntegral outputHeight) * (windowEndY - windowStartY)

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
grayscale iters = 255 - round (255 * (fromIntegral iters / fromIntegral maxIters))

getPixelColor :: Int -> Int -> PixelRGB8
getPixelColor x y = result
  where
    result = PixelRGB8 (fromIntegral v) (fromIntegral v) (fromIntegral v)
    v = grayscale (getIters (coordToComplex x y))

genImage :: Int -> Int -> Image PixelRGB8
genImage = generateImage getPixelColor

main :: IO ()
main = writePng "output.png" (genImage outputWidth outputWidth)