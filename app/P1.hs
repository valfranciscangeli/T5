module Main where
import Data.Array ((!), bounds, listArray, Array)
import Control.Parallel.Strategies( rpar,  runEval)
import Control.Exception()
import GHC.Arr ()
import Data.List.Split (chunksOf)
import qualified Control.Applicative as 1

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
-- lo mismo del enunciado para r_i, pero en codigo Haskell
coeffProd :: Poly -> Poly -> Int -> Coeff
coeffProd p1 p2 i = sum [coeff p1 j * coeff p2 (i - j) | j <- [0..i]]

seqProd :: Poly -> Poly -> Poly
seqProd p1 p2 = mkPoly (map (coeffProd p1 p2) [0..d])  where
  d = degree p1 + degree p2


-- parte (b)
-- se ejecuta con: cabal run p1-exe +RTS -s
-- al ejecutar en diferentes ocasiones los resultados eran muy variados asi que 
-- se toma como resultado el promedio de 10 ejecuciones

-- Elapsed time: 0.264s

-- parte (c)

parProd :: Poly -> Poly -> Poly
parProd p1 p2 = mkPoly (concat coeffs)
  where
    d = degree p1 + degree p2
    indices = [0..d]
    groupedIndices = chunksOf 15 indices
    coeffs = runEval $ mapM (rpar . map (coeffProd p1 p2)) groupedIndices


-- parte (d)
-- se ejecuta con: cabal run p1-exe +RTS -N2 -s
-- se toma como resultado el promedio de 10 ejecuciones

-- Elapsed time: 0.185s


-- parte (e)
-- Speedup: se calcula el speedup como:
{-
  Speedup = Tiempo secuencial / Tiempo paralelo
  Speedup = 0.264s / 0.185s = 1,427
-}


-- parte (f)
par1Prod :: Poly -> Poly -> Poly
par1Prod p1 p2 = mkPoly coeffs
  where
    d = degree p1 + degree p2
    coeffs = runEval $ mapM (rpar . coeffProd p1 p2) [0..d]


-- parte (g)
-- se ejecuta con: cabal run p1-exe +RTS -N2 -s
-- se toma como resultado el promedio de 10 ejecuciones

-- Elapsed time: 0.187s
-- Speedup: 1,411 

-- parte (h)
{-
Al comparar el calculo secuencial con la primera version paralela se tiene un speedup mucho menor al esperado en teoria, que seria el doble. En este caso, esto se puede deber a que se tienen otros procesos necesarios que no pueden ser paralelizados, como es el dividir la lista de indices en sublistas de 15 elementos y el concatenar los resultados para cada sublista para obtener el resultado final. Estos procesos suman un costo fijo que no estaba en la implementacion secuencial original. A esto, se suma la creacion de los trabajos o sparks para calcular los coeficientes para cada sublistas, que tienen un costo en si mismos. 
Lo mismo ocurre con el speedup de la segunda version paralela, donde hay casi nula diferencia con la primera version- Esto se explica principalmente por la sobrecarga del codigo con la creacion de sparks para cada uno de los elementos en la lista de indices, manteniendo el uso de solo 2 cores, provocando que las tareas deban ir quedando "en cola" y no se resuelvan todas de forma paralela.
-}

-- parte (i)
{-
Esto se realiza porque el producto de polinimios genera una lista con 4900 coeficientes, lo cual ocupa una gran cantidad de espacio en la consola y añade tiempo de ejecucion que podria afectar los resultados. Sin embargo, no es necesario aplicarla si el fin esforzar la evaluacion del calculo del producto en general, ya que, como se necesita mostrar el resultado en pantalla, el calculo es forzado por la funcipon print. 
En el caso de las funciones  creadas para el calculo paralelo, se fuerza el calculo en el mismo retorno, ya que debia retornar para poder usar la sintaxis do, pero si no fuera este caso, el print tambien forzaria el calculo del producto. Es diferente al caso visto en clases, ya que la evaluacion lazy provocaria que en el ejemplo donde se imprime el largo de la lista resultante, esta no se evalue, ya que no es necesario conocer el contenido de la lista para calcular su largo. 
-}


-- Determina el numero de coeficientes no nulos de un polinomio
nonNullCoeff :: Poly -> Int
nonNullCoeff = foldr (\c rec -> if c == 0 then rec else rec + 1) 0

main :: IO()
main = do
  let pa = mkPoly [100..2000]
      pb = mkPoly [2000..5000]

  -- Parte (b)
  --print (nonNullCoeff (seqProd pa pb))

  -- Parte (d)
  --print (nonNullCoeff (parProd pa pb))

  -- Parte (g)
  --print (nonNullCoeff (par1Prod pa pb))

  return ()
