module Kcoloracion where

import SAT.MiniSat 
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (catMaybes)
import Data.List (findIndex)



type Vertice = Int
type Arista = (Vertice, Vertice)
type Grafica = ([Vertice], [Arista])
-- type Color = Int
type Coloracion = [(Vertice, Int)]
-- type Coloracion = [(Vertice, Color)]

-- Función para crear una gráfica vacía
crearGraficaVacia :: Grafica
crearGraficaVacia = ([], [])

-- Función para crear una gráfica vacía que solo contenga vértices, pero no aristas 
crearGrafica :: Int -> Grafica
crearGrafica n = ([1..n], [])

-- Función para agregar una arista a la gráfica
agregarArista :: Arista -> Grafica -> Grafica
agregarArista arista (vertices, aristas) = (vertices, arista : aristas)

-- Función para obtener los vértices de la gráfica
obtenerVertices :: Grafica -> [Vertice]
obtenerVertices (vertices, _) = vertices

-- Función para obtener las aristas de la gráfica
obtenerAristas :: Grafica -> [Arista]
obtenerAristas (_, aristas) = aristas


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

-- Función para resolver la fórmula usando MiniSat
resolverSAT :: [[Int]] -> IO [[Int]]
resolverSAT formula = solve formula


-- Función para construir las k-coloraciones a partir de la solución del SAT solver
construirColoraciones :: Int -> Grafica -> [[Int]] -> [[Coloracion]]
construirColoraciones k grafica soluciones = map (construirColoracion k vertices) soluciones
  where
    vertices = obtenerVertices grafica

-- Función para construir una k-coloración a partir de una solución SAT
construirColoracion :: Int -> [Vertice] -> [Int] -> Coloracion
construirColoracion k vertices solucion = catMaybes [buscarColor v solucion | v <- vertices]
  where
    buscarColor v sol = findIndex (\i -> c v i `elem` sol) [1..k]
    c i j = (i - 1) * k + j

-- Función para obtener todas las k-coloraciones de una gráfica
kColoracion :: Int -> Grafica -> IO [[Coloracion]]
kColoracion k grafica = do
    let vertices = obtenerVertices grafica
        aristas = obtenerAristas grafica
        formula = generarFormula k (length vertices) aristas
    solucionesSAT <- resolverSAT formula
    return $ construirColoraciones k grafica solucionesSAT







