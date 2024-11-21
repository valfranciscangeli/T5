module Main where

import Control.Parallel.Strategies (parList, rpar, runEval)
import Data.List.Split (chunksOf)

type Number = Double

-- parte (a)
integral :: (Number -> Number) -> Number -> Number -> Int -> Number
integral f a b n = h2 * sumatoria
  where
    h = (b - a) / fromIntegral n
    h2 = h / 2
    sumatoria = sum [f (a + fromIntegral i * h) + f (a + (fromIntegral i + 1) * h) | i <- [0 .. n]]

-- parte (b)
integralaux :: (Number -> Number) -> Number -> Number -> [Int] -> Number
integralaux f a h ies = h2 * sumatoria
  where
    h2 = h / 2
    sumatoria = sum [f (a + fromIntegral i * h) + f (a + (fromIntegral i + 1) * h) | i <- ies]

pintegral :: (Number -> Number) -> Number -> Number -> Int -> Number
pintegral f a b n = sum resultado
  where
    indices = [0 .. n]
    h = (b - a) / fromIntegral n
    groupedIndices = chunksOf 50 indices
    resultado = runEval $ parList rpar $ map (integralaux f a h) groupedIndices

-- parte (c)
-- se ejecuta con: cabal run p2-exe +RTS -N2 -s
-- se toma como resultado el promedio de 10 ejecuciones

-- Elapsed time sequential version: 1,080s
-- Elapsed time parallel version: 1,094s
-- Speedup: 0,987

main :: IO ()
main = do
  let f = \x -> 2 * x ^ 2 + 3 * x ^ 10 - x ^ 6 + 10 * x ^ 30 - 8 * x ^ 25
      a = 0
      b = 100
      n = 20000

  -- secuencial ==========
  --print (integral f a b n)    

  -- paralelo ============
  print (pintegral f a b n)

  return ()
