-- Declaramos el módulo Main.

-- Este módulo se encarga de la interacción con el usuario.
-- Aquí se incluyen funciones para leer la entrada del usuario, construir la 
-- gráfica, convertirla a una fórmula SAT, y mostrar la coloración
-- (si es que existe) o en caso contrario, dice que no existe.
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



-- Función para leer una gráfica desde la entrada estándar, solicitando al
-- usuario que ingrese una gráfica, es decir sus aristas y vértices.
leerGrafica :: IO Grafica
leerGrafica = do
    putStrLn "Ingresa los vértices separados por espacios (ej. '1 2 3' para tres vertices llamados (1,2,3) respectivamente):"
    verticesLine <- getLine
    let vertices = map read (words verticesLine)
    
    putStrLn "Ingresa las aristas como pares de vértices separados por espacios (ej. '1 2 2 3 3 1' para aristas (1,2), (2,3) y (3,1)):"
    aristasLine <- getLine
    let aristas = leerAristas (words aristasLine)
    
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
    putStrLn "==========================================="
    putStrLn "==== Proyecto Final: HASKELL + MINISAT ===="
    putStrLn "==========================================="
    putStrLn "                                           "
    putStrLn "-------------------------------------------"
    putStrLn "========== COLORACIÓN DE GRÁFICAS ========="
    putStrLn "-------------------------------------------"
    putStrLn "                                           "
    putStrLn "Ingresa el número de colores (k):"

    kLine <- getLine
    let k = fromMaybe 0 (readMaybe kLine :: Maybe Int)
    
    if k <= 0
    then putStrLn "Número de colores inválido. Debe ser mayor a 0."
    else do
        grafica <- leerGrafica
        case solveKcoloracion k grafica of
            Nothing -> putStrLn "No es posible encontrar una k-coloración para la gráfica dada."
            Just coloracion -> do
                putStrLn "La k-coloración encontrada es:"
                mostrarColoracion (toList coloracion)
