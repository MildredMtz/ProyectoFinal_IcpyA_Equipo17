module Main where
import SAT.MiniSat
import Kcoloracion

main :: IO ()
main = do
    let grafica = agregarArista (1, 2) $ agregarArista (2, 3) $ agregarArista (3, 1) $ crearGrafica 3
    let k = 3
    let coloraciones = kColoracion k grafica
    print coloraciones
  

 
