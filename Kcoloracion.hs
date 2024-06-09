-- kcoloracion.hs

-- Declaramos el módulo Kcoloracion y los elementos que exporta.
module Kcoloracion where

-- Importamos el módulo SAT.MiniSat para trabajar con fórmulas SAT.
import SAT.MiniSat (Formula(..), solve)

-- Importamos el módulo Data.Map y lo calificamos como Map para evitar conflictos de nombres.
import qualified Data.Map as Map
-- Importamos el tipo Map desde Data.Map.
import Data.Map (Map)

-- Definimos un tipo alias Vertice para representar vértices.
type Vertice = Int
-- Definimos un tipo alias Arista para representar aristas (pares de vértices).
type Arista = (Vertice, Vertice)
-- Definimos un tipo alias Grafica que es una lista de vértices y una lista de aristas.
type Grafica = ([Vertice], [Arista])
-- Definimos un tipo alias Color para representar colores.
type Color = Int
-- Definimos un tipo alias Coloracion para representar una asignación de colores a vértices.
type Coloracion = Map Vertice Color

-- Función que convierte un problema de k-coloración en una fórmula SAT.
kColoracionFormula :: Int -> Grafica -> Formula (Vertice, Color)
kColoracionFormula k (vertices, aristas) = All (
    -- Cada vértice debe tener exactamente un color.
    [ExactlyOne [Var (v, c) | c <- [1..k]] | v <- vertices] ++
    -- Dos vértices adyacentes no pueden tener el mismo color.
    [Not (Var (v, c)) :||: Not (Var (u, c)) | (v, u) <- aristas, c <- [1..k]]
    )

-- Función que resuelve el problema de k-coloración utilizando el solucionador SAT.
solveKcoloracion :: Int -> Grafica -> Maybe Coloracion
solveKcoloracion k grafica = case solve (kColoracionFormula k grafica) of
    -- Si hay una solución, la convertimosa un formato legible.
    Just solMap -> Just $ Map.fromList [(v, c) | ((v, c), True) <- Map.toList solMap]
    -- Si no hay solución, devolvemos Nothing.
    Nothing -> Nothing
