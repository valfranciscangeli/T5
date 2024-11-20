module P2 where


type Number = Double

-- parte (a)
integral :: (Number -> Number) -> Number -> Number -> Int -> Number
integral = undefined -- complete aquí su código


-- parte (b)
pintegral :: (Number -> Number) -> Number -> Number -> Int -> Number
pintegral = undefined -- complete aquí su código


-- parte (c)
-- Elapsed time sequential version: complete aquí
-- Elapsed time parallel version: complete aquí
-- Speedup: complete aquí


main :: IO()
main = do
  let f = \x -> 2*x^2 + 3*x^10 - x^6 + 10*x^30 - 8*x^25
      a = 0
      b = 100
      n = 20000
  print (pintegral f a b n)
  return ()
