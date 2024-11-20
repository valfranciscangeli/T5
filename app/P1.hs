import Data.Array
import Control.Parallel.Strategies
import Control.Exception

type Coeff = Integer
type Poly = Array Int Coeff

mkPoly :: [Coeff] -> Poly
mkPoly xs = listArray (0,d) xs where d = length xs - 1

degree :: Poly -> Int
degree = snd . bounds

coeff :: Poly -> Int -> Coeff
coeff p i | i <= degree p = p ! i
          | otherwise     = 0


-- parte (a)
coeffProd :: Poly -> Poly -> Int -> Coeff
coeffProd = undefined  -- agregue aquí su definición

seqProd :: Poly -> Poly -> Poly
seqProd p1 p2 = mkPoly (map (coeffProd p1 p2) [0..d])  where
  d = degree p1 + degree p2


-- parte (b)
-- Elapsed time: complete aquí


-- parte (c)
parProd :: Poly -> Poly -> Poly
parProd p1 p2 = undefined  -- agregue aquí su definición


-- parte (d)
-- Elapsed time: complete aquí


-- parte (e)
-- Speedup: complete aquí


-- parte (f)
par1Prod :: Poly -> Poly -> Poly
par1Prod p1 p2 == undefined  -- agregue aquí su definición

-- parte (g)
-- Elapsed time: complete aquí
-- Speedup: complete aquí

-- parte (h)
{-
Complete aquí
-}

-- parte (i)
{-
Complete aquí
-}


-- Determina el número de coeficientes no nulos de un polinomio
nonNullCoeff :: Poly -> Int
nonNullCoeff = foldr (\c rec -> if (c == 0) then rec else rec + 1) 0

main :: IO()
main = do
  let pa = mkPoly [100..2000]
      pb = mkPoly [2000..5000]
  print (nonNullCoeff (seqProd pa pb))
  return ()
