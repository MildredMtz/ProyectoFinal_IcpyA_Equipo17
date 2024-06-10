-- Declaramos el módulo Kcoloracion

-- Este módulo define los tipos y funciones necesarios, representa una gráfica y 
-- resolver el problema de k-coloración utilizando un solucionador SAT. 
-- Proporciona funciones para convertir un problema de k-coloración en una fórmula SAT.

module Kcoloracion where

-- Importamos el módulo SAT.MiniSat para trabajar con fórmulas SAT.
import SAT.MiniSat (Formula(..), solve)

-- Importamos el módulo Data.Map y lo calificamos como Map para evitar conflictos de nombres.
import qualified Data.Map as Map
-- Importamos el tipo Map desde Data.Map.
import Data.Map (Map)

-- Tipo Vertice, representa vértices.
type Vertice = Int
-- Tipo Arista, representa aristas (pares de vértices).
type Arista = (Vertice, Vertice)
-- Tipo Grafica que es una lista de vértices y una lista de aristas.
type Grafica = ([Vertice], [Arista])
-- Tipo Color, representa colores.
type Color = Int
-- Tipo Coloracion, representa una asignación de colores a vértices.
type Coloracion = Map Vertice Color

-- Función que convierte un problema de k-coloración en una fórmula SAT,
-- para la cual recibe el número de colores (k) y una gráfica, y devuelve una fórmula SAT.
kColoracionFormula :: Int -> Grafica -> Formula (Vertice, Color)
kColoracionFormula k (vertices, aristas) = All (
    -- Cada vértice debe tener exactamente un color.
    [ExactlyOne [Var (v, c) | c <- [1..k]] | v <- vertices] ++
    -- Dos vértices adyacentes no pueden tener el mismo color.
    [Not (Var (v, c)) :||: Not (Var (u, c)) | (v, u) <- aristas, c <- [1..k]]
    )

-- Función que resuelve el problema de k-coloración utilizando el solucionador SAT,
-- para lo cual recibe el número de colores (k) y una gráfica, posteriormente devuelve una posible coloración,
-- o Nothing si no es posible encontrar una k-coloración.
solveKcoloracion :: Int -> Grafica -> Maybe Coloracion
solveKcoloracion k grafica = case solve (kColoracionFormula k grafica) of
    Just solMap -> Just $ Map.fromList [(v, c) | ((v, c), True) <- Map.toList solMap]
    Nothing -> Nothing
