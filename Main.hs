-- main.hs

-- Declaramos el módulo Main.
module Main where

-- Importamos la función y los tipos necesarios del módulo Kcoloracion.
import Kcoloracion (Grafica, solveKcoloracion, Vertice, Arista, Color)
-- Importamos la función toList de Data.Map para convertir un Map a una lista.
import Data.Map (toList)
-- Importamos la función fromMaybe de Data.Maybe para manejar valores Maybe.
import Data.Maybe (fromMaybe)
-- Importamos la función forM_ de Control.Monad para iterar sobre listas.
import Control.Monad (forM_)
-- Importamos la función readMaybe de Text.Read para convertir cadenas a números de forma segura.
import Text.Read (readMaybe)

-- Función para leer una gráfica desde la entrada estándar.
leerGrafica :: IO Grafica
leerGrafica = do
    -- Pedimos al usuario que ingrese los vértices.
    putStrLn "Ingresa los vértices separados por espacios:"
    verticesLine <- getLine
    -- Leemos los vértices como una lista de enteros.
    let vertices = map read (words verticesLine)
    
    -- Pedimos al usuario que ingrese las aristas.
    putStrLn "Ingresa las aristas como pares de vértices separados por espacios (ej. '1 2 3 4' para aristas (1,2) y (3,4)):"
    aristasLine <- getLine
    -- Leemos las aristas utilizando la función auxiliar leerAristas.
    let aristas = leerAristas (words aristasLine)
    
    -- Devolvemos la gráfica como una tupla de vértices y aristas.
    return (vertices, aristas)

-- Función auxiliar para convertir una lista de cadenas a una lista de aristas.
leerAristas :: [String] -> [Arista]
leerAristas [] = []
leerAristas (x:y:rest) = (read x, read y) : leerAristas rest
leerAristas _ = []

-- Función para mostrar la coloración resultante.
mostrarColoracion :: [(Vertice, Color)] -> IO ()
mostrarColoracion coloracion = forM_ coloracion $ \(v, c) ->
    putStrLn $ "Vertice " ++ show v ++ " -> Color " ++ show c

-- Función principal que maneja la interacción con el usuario.
main :: IO ()
main = do
    -- Mostramos el menú inicial.
    putStrLn "==== K-Coloracion Solver 1.0.0. ===="
    putStrLn "Ingresa el número de colores (k):"
    -- Leemos el número de colores.
    kLine <- getLine
    -- Intentamos convertir el input a un número.
    let k = fromMaybe 0 (readMaybe kLine :: Maybe Int)
    
    -- Si el número de colores es inválido, mostramos un mensaje de error.
    if k <= 0
    then putStrLn "Número de colores inválido. Debe ser mayor a 0."
    else do
        -- Leemos la gráfica desde la entrada estándar.
        grafica <- leerGrafica
        -- Intentamos resolver el problema de k-coloración.
        case solveKcoloracion k grafica of
            -- Si no hay solución, mostramos un mensaje de error.
            Nothing -> putStrLn "No es posible encontrar una k-coloración para la gráfica dada."
            -- Si hay solución, mostramos la coloración.
            Just coloracion -> do
                putStrLn "La k-coloración encontrada es:"
                mostrarColoracion (toList coloracion)
