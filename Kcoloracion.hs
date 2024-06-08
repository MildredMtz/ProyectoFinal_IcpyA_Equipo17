--module Kcoloracion (
 --   Grafica,
  --  Coloracion,
  --  crearGrafica,
  --  agregarArista,
  --  kColoracion
--) where

module Kcoloracion where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Process (callCommand)


type Vertice = Int
type Arista = (Vertice, Vertice)
type Grafica = ([Vertice], [Arista])
-- type Color = Int
type Coloracion = [(Vertice, Int)]
-- type Coloracion = [(Vertice, Color)]



-- Crear una gráfica vacía
crearGrafica :: Int -> Grafica
crearGrafica n = ([1..n], [])

-- Agregar una arista a una gráfica
agregarArista :: Arista -> Grafica -> Grafica
agregarArista arista (vertices, aristas) = (vertices, arista : aristas)


-- Paso 4: Definir la función kColoracion
kColoracion :: Int -> Grafica -> [[Coloracion]]
kColoracion k (vertices, aristas) = let
    formulas = generarFormula k vertices aristas
    solucionesSAT = resolverSAT formulas
    in map (construirColoracion vertices k) solucionesSAT

-- Generar la fórmula proposicional
generarFormula :: Int -> [Vertice] -> [Arista] -> [[Int]]
generarFormula k vertices aristas = concat
    [ -- Cada vértice tiene al menos un color
      [[x i j | j <- [1..k]] | i <- vertices]
    , -- Cada vértice tiene a lo sumo un color
      [[-x i j, -x i l] | i <- vertices, j <- [1..k], l <- [j+1..k]]
    , -- Vértices adyacentes tienen diferentes colores
      [[-x u j, -x v j] | (u, v) <- aristas, j <- [1..k]]
    ]
  where
    x i j = (i - 1) * k + j


-- Resolver la fórmula usando MiniSat
resolverSAT :: [[Int]] -> [[Int]]
resolverSAT formula = unsafePerformIO $ do
    (inp, out, err, pid) <- runInteractiveProcess "minisat" [] Nothing Nothing
    hPutStr inp (unlines (map (unwords . map show) formula ++ [""]))
    hClose inp
    output <- hGetContents out
    return (parseOutput output)
  where
    parseOutput output = case lines output of
        ("SAT":solution:_) -> [map read (words solution)]
        _ -> []


-- Construir la k-coloración a partir de la solución del SAT solver
construirColoracion :: [Vertice] -> Int -> [Int] -> Coloracion
construirColoracion vertices k solucion = catMaybes $ do
    vertice <- vertices
    let colores = [1..k]
    return $ asum $ map (\color -> if (vertice - 1) * k + color `elem` solucion then Just (vertice, color) else Nothing) colores


-- IMPLEMENTAR FUNCION K-Coloracion
