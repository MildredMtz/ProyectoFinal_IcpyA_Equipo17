Equipo 17 conformado por:

Martínez Hidalgo Paola Mildred - 319300217
Osorio López Claudia Yarinet   - 316256098
López Villalba Cielo           - 422050461

# Proyecto Final - Versión A (HASKELL + MINISAT)

## Consideraciones importantes
Para ejecutar este proyecto, es necesario contar con la versión más actualizada de cabal. Esto se puede hacer corriendo en terminal:
    - ghcup upgrade
    - ghcup install ghc 9.10.1
    - ghcup install cabal 3.10.3.0
    - ghcup install stack 2.15.7

Debido a la versión que tenemos de ghc, la biblioteca SAT no es compatible, pero, lo resolvimos utilizando una versión diferente del paquete SAT, es decir satchmo.
    - cabal install satchmo

## Descripción General 

En este proyecto se pretende la implementación de un solucionador del problema de la k-coloración de gráficas en Haskell, utilizando un solucionador SAT (problema de satisfacibilidad booleana) para encontrar soluciones de manera eficiente.
La idea principal gira en torno a convertir el problema de la coloración de gráficas en un problema SAT, donde las restricciones del mismo se traducen en fórmulas lógicas. 
Estas fórmulas se alimentan luego a un solucionador SAT, que verifica la satisfacibilidad y devuelve una solución que satisface todas las restricciones dadas si existe una. 

Es importante recalcar que, este corresponde a un problema NP-completo, lo cual significa que no se ha encontrado hasta el momento un algoritmo que nos permita obtener una respuesta óptima, sino, buenas aproximaciones, que, aunque se pulen cada vez más, no se puede demostrar que sea la forma más óptima de su resolución.

## Detalles de la Implementación

### Kcoloracion.hs

El módulo "Kcoloracion.hs" es en donde reside la lógica principal para transformar una gráfica coloreable en un problema SAT. Los componentes clave de este módulo incluyen:

- **Tipos de Datos**:
- **Tipos de Datos**:
- **Tipos de Datos**:

### Main.hs
El módulo `Main.hs` maneja la interacción con el usuario, permitiendo la entrada de gráficas y mostrando las soluciones encontradas. Se encarga de:

- 
- 
- 



## Razón de la Implementación

La elección de un solucionador SAT para resolver Sudokus se basa en la eficiencia y potencia de los solucionadores SAT modernos, que pueden manejar rápidamente problemas de satisfacibilidad complejos. Convertir las reglas de Sudoku en fórmulas lógicas permite aprovechar estas capacidades, encontrando soluciones a puzzles de Sudoku que serían difíciles y lentos de resolver mediante métodos de fuerza bruta.

## Conclusiones

La implementación de este solucionador de Sudoku demuestra la flexibilidad y potencia de los solucionadores SAT, así como la elegancia de Haskell para manipular estructuras de datos y lógica compleja. Este proyecto sirve como un ejemplo práctico de cómo los problemas del mundo real pueden ser modelados y resueltos mediante técnicas de programación lógica y satisfacibilidad.

