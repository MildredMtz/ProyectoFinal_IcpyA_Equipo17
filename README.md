Equipo 17 conformado por:

Martínez Hidalgo Paola Mildred - 319300217
Osorio López Claudia Yarinet   - 316256098
López Villalba Cielo           - 422050461

# Proyecto Final - Versión A (HASKELL + MINISAT)

## COLORACIÓN DE GRÁFICAS - PROBLEMA DE LA K-COLORACIÓN DE GRÁFICAS

## Consideraciones importantes
Para ejecutar este proyecto, es necesario contar con :

    -> ghcup
        pues es es el instalador principal del lenguaje de propósito general Haskell.
            Para más información visitar:
            https://www.haskell.org/ghcup/

GHCup nos servirá para instalar las herramientas ghc y cabal.
    - Fedora:
      -  sudo dnf install ghc cabal-install

    -> GHC es el compilador nativo de código libre para el lenguaje de programación funcional Haskell.
        Para más información visitar:
        https://es.wikipedia.org/wiki/Glasgow_Haskell_Compiler#:~:text=El%20Glasgow%20Haskell%20Compiler%20(o,Peyton%20Jones%20y%20Simon%20Marlow.

    -> Cabal (Common Architecture for Building Applications and Libraries) es una API distribuida con GHC, NHC98 y Hugs que permite a un desarrollador agrupar fácilmente un conjunto de módulos para producir un paquete.

    Además se ocupa la versión más actualizada de cabal. Esto se puede hacer corriendo en terminal:
        - ghcup upgrade
        - ghcup install ghc 9.10.1
        - ghcup install cabal 3.10.3.0
        - ghcup install stack 2.15.7

        Para más información visitar:
        https://www.haskell.org/cabal/


-> MiniSat

    Finalmente, necesitamos la biblioteca MiniSat, pues este módulo proporciona enlaces Haskell de alto nivel para el conocido solucionador de satisfacibilidad MiniSat. 
    Es decir, resuelve el problema de satisfacibilidad booleana.
    Es un solucionador SAT de propósito general totalmente automatizado y bien optimizado escrito por Niklas Een y Niklas Sorensson, y modificado por Takahisa Toda.

    Esto se puede hacer corriendo en terminal:
        - cabal update
        - cabal install --lib minisat-solver

        Para más información, visitar: 
        https://hackage.haskell.org/package/minisat-solver-0.1/docs/SAT-MiniSat.html



## Descripción General 

En este proyecto se pretende la implementación de un solucionador del problema de la k-coloración de gráficas en Haskell, utilizando un solucionador SAT (problema de satisfacibilidad booleana) para encontrar soluciones de manera eficiente.
La idea principal gira en torno a convertir el problema de la coloración de gráficas en un problema SAT, donde las restricciones del mismo se traducen en fórmulas lógicas. 
Estas fórmulas se alimentan luego a un solucionador SAT, que verifica la satisfacibilidad y devuelve una solución que satisface todas las restricciones dadas si existe una. 

Es importante recalcar que, este corresponde a un problema NP-completo, lo cual significa que no se ha encontrado hasta el momento un algoritmo que nos permita obtener una respuesta óptima, sino, buenas aproximaciones, que, aunque se pulen cada vez más, no se puede demostrar que sea la forma más óptima de su resolución.

## Detalles de la Implementación

### Kcoloracion.hs

El módulo "Kcoloracion.hs" es en donde reside la lógica principal para representar una gráfica y transformarla en una gráfica coloreable (si es que que ésta posee alguna coloración) utilizando un solucionador SAT. Los componentes clave de este módulo incluyen:

- **Tipos de Datos**: 
    * Vértice, representa los vértices de una gráfica a través de un entero 'int'.
    * Arista, representa pares de vértices.
    * Grafica, representa una gráfica, es decir una lista de vértices y otra de aristas. 
    * Color,   representa un color a través de un entero 'int'.
    * Coloracion, representa la asignación de colores a los vértices de una gráfica.

- **kColoracionFormula**: Función que recibe un número k (lo que corresponde con el número de colores) y una gráfica, a partir de cual devuelve una fómula SAT. En ella se ecentran las condiciones necesarias para la solución al problema de coloración de gráficas:
    - Cada vértice debe tener exactamente un color.
    - Vértices adyacentes no pueden tener el mismo color.

- **solveKcoloracion**: Función que utiliza el solucionador SAT para resolver el problema de la k-coloración. Recibe el número de colores, una gráfica y devuelve una posible coloración si es que existe.

### Main.hs
El módulo `Main.hs` maneja la interacción con el usuario, permitiendo la entrada de gráficas y mostrando las soluciones encontradas. Se encarga de:

- Recoger la gráfica y número de colores del usuario.
- Llama a solveKcoloracion con la gráfica y número de colores ingresados.
- Muestra la solucion al usuario si es que existe.

## Razón de la Implementación

La elección de un solucionador SAT para resolver el problema de la k-coloración de gráficas se basa en la eficiencia y potencia de los solucionadores SAT modernos, que pueden manejar rápidamente problemas de satisfacibilidad complejos, pues aunque este es un problema np-completo, logra dar una respuesta de forma rápida, que, puede nos ser la óptima, pero sí una buena aproximación.
Convertir las reglas del problema de coloración de gráficas en fórmulas lógicas permite aprovechar estas capacidades, encontrando soluciones a este problema que serían difíciles y lentos de resolver mediante métodos de fuerza bruta.

## Conclusiones

La implementación de este solucionador del problema de la k-coloración de gráficas demuestra la flexibilidad y potencia de los solucionadores SAT, así como la elegancia de Haskell para manipular estructuras de datos y lógica compleja. Este proyecto sirve como un ejemplo práctico de cómo algunos problemas np-completos del mundo real encuentran una solución no optimizada, pero útil, pues pueden ser modelados y resueltos mediante técnicas de programación lógica y satisfacibilidad.

