-- Tarea 3 - Hugo Farías (258218)
module Tarea3 where

import Debug.Trace

-- Ejercicio 1

type Simbolo = String
type Current = Simbolo

type Estado = String

type Cinta = ([Simbolo], Current, [Simbolo])

type Rama = (Simbolo, (Accion, Estado))

data Accion = L
            | R
            | W Simbolo
            deriving (Show)

type Codigo = [(Estado, [Rama])]

-- Ejercicio 2

-- Devuelve el ultimo elemento de una lista
ultimo :: [a] -> a
ultimo (x:[]) = x
ultimo (x:xs) = ultimo xs

-- Elimina el ultimo elemento de una lista
eliminarUltimo :: [a] -> [a]
eliminarUltimo (x:[]) = []
eliminarUltimo (x:xs) = x:eliminarUltimo xs

left :: Cinta -> Cinta
left ([], c, r) = (["#"], "#", c:r)
left (l, c, r) = ((eliminarUltimo l), (ultimo l), c:r)

right :: Cinta -> Cinta
right (l, c, []) = (l ++ ["#"], "#", [])
right (l, c, (r:rs)) = (l ++ [c], r, rs) 

write :: Simbolo -> Cinta -> Cinta
write s (l, c, r) = (l, s, r)

-- Dado un estado, devuelve su tabla de ramas
dameRamas :: Estado -> Codigo -> [Rama]
dameRamas e ((e', r):rs)
    | e == e' = r
    | otherwise = dameRamas e rs

-- Funcion para buscar una accion en una lista de ramas
buscarRama :: Simbolo -> [Rama] -> (Accion, Estado) 
buscarRama s ((s', (a, e)):rs)
    | s' == "_" = (a, e)
    | s == s' = (a, e)
    | otherwise = buscarRama s rs

step :: Estado -> Codigo -> Cinta -> (Cinta, Estado)
step e codigo (l, c, r) = case buscarRama c (dameRamas e codigo) of
    (L, e') -> ((left (l, c, r)), e')
    (R, e') -> ((right (l, c, r)), e')
    (W s, e') -> ((write s (l, c, r)), e')

iteracion :: Estado -> Codigo -> Cinta -> (Cinta, Estado)
iteracion e codigo cinta = case e of 
    "h" -> (cinta, "h")
    _ -> iteracion e' codigo cinta'
    where (cinta', e') = step e codigo cinta

ejecucion :: Codigo -> Cinta -> (Cinta, Estado)
ejecucion codigo cinta = iteracion "i" codigo cinta

-- Ejercicio 3

------------
-- LSgima --
------------

-- Precondicoiones: no tiene. Si no se encuentra el simbolo sigma en la cinta, se devuelve mensaje de error.

sigma :: Simbolo
sigma = "Sigma"

lSigma :: Codigo 
lSigma = [
        ("i", [
            ("_", (L, "loop"))
        ]),
        ("loop", [
            (sigma, (W sigma, "h")),
            ("#", (W "#", "h")), -- En caso de no encontraron el simbolo sigma
            ("_", (L, "loop"))
        ])
    ]

cintaInicial :: Cinta
cintaInicial = (["a", sigma, "a", "c"], "#", ["c", "d", "c", "#"])

ejecutarLSigma = ejecucion lSigma cintaInicial -- Para ejecutar LSigma

---------
-- Par --
---------

{- Precondicion: Partimos de un simbolo # como current. A la izquierda se encuentra la tira de I para la cual
queremos saber si es par o impar. A la derecha se encuentran el resiultado, T si es par y F si es impar.

Ejemplo 
Cinta inicial: (I, I, I, I, #)
                            ^                        
                         current

Cinta final: (I, I, I, I, #, T)
                          ^  
                        current
-}

par :: Codigo
par = [
        ("i", [
            ("#", (L, "estadoPar"))
        ]),
        ("estadoPar", [
            ("I", (L, "estadoImpar")),
            ("#", (R, "loopT"))
        ]),
        ("loopT", [
            ("#", (R, "q1")),
            ("I", (R, "loopT"))
        ]),
        ("q1", [
            ("#", (W "T", "q2"))
        ]),
        ("q2", [
            ("T", (L, "h"))
        ]),
        ("estadoImpar", [
            ("I", (L, "estadoPar")),
            ("#", (R, "loopF"))
        ]),
        ("loopF", [
            ("#", (R, "q3")),
            ("I", (R, "loopF"))
        ]),
        ("q3", [
            ("#", (W "F", "q4"))
        ]),
        ("q4", [
            ("F", (L, "h"))
        ])  
    ]

cintaPar :: Cinta
cintaPar = (["I", "I", "I", "I"], "#", [])

ejecutarPar = ejecucion par cintaPar -- Para ejecutar Par

----------
-- Elem --
----------

{- Precondicion: Partimos de un simbolo # como current. A la izquierda se encuentra la tira donde buscamos
el simbolo sigma. A la derecha se encuentra el resultado, T si el simbolo sigma se encuentra en la tira y F
si no se encuentra.

Ejemplo 
Cinta inicial: ("a", "Sigma", "a", "a", #)
                                        ^                        
                                      current

Cinta final: ("a", "Sigma", "a", "a", #, T)
                                      ^  
                                    current
-}

sigmaElem :: Simbolo
sigmaElem = "Sigma"

elemMT :: Codigo
elemMT = [
        ("i", [
            ("#", (L, "sigo"))
        ]),
        ("sigo", [
            (sigmaElem, (W sigmaElem, "volverTrue")),
            ("#", (R, "volverFalse")),
            ("_", (L, "sigo"))
        ]),
        ("volverTrue", [
            ("#", (R, "true")),
            ("_", (R, "volverTrue"))
        ]),
        ("true", [
            ("#", (W "T", "termino"))
        ]),
        ("volverFalse", [
            ("#", (R, "false")),
            ("_", (R, "volverFalse"))
        ]),
        ("false", [
            ("#", (W "F", "termino"))
        ]),
        ("termino", [
            ("_", (L, "h"))
        ])
    ]

cintaElem :: Cinta
cintaElem = (["a", sigma, "a", "a"], "#", [])

ejecutarElem = ejecucion elemMT cintaElem -- Para ejecutar Elem

-------------
-- Reverse --
-------------

{- Precondicion: Partimos de un simbolo # como current. A la izquierda se encuentra la tira. A la derecha
se encuentra la tira donde se guardara la tira original invertida.

Ejemplo 
Cinta inicial: (a, b, a, a, #)
                            ^                        
                          current

Cinta final: (a, b, a, a, #, a, a, b, a, #)
                          ^  
                        current
-}

sigma1 :: Simbolo
sigma1 = "a"

sigma2 :: Simbolo
sigma2 = "b"

reverseMT :: Codigo
reverseMT = [
            ("i", [
                ("#", (L, "primerPaso"))
            ]),
            ("primerPaso", [
                ("#", (R, "centroFinal")),
                (sigma1, (W "@", "vuelvoCentroDerechaS1")),
                (sigma2, (W "@", "vuelvoCentroDerechaS2"))
            ]),
            ("vuelvoCentroDerechaS1", [
                ("#", (R, "finDerechaS1")),
                ("_", (R, "vuelvoCentroDerechaS1"))    
            ]),
            ("vuelvoCentroDerechaS2", [
                ("#", (R, "finDerechaS2")),
                ("_", (R, "vuelvoCentroDerechaS2"))    
            ]),
            ("finDerechaS1", [
                ("#", (R, "vuelvoUnoSigma1")),
                ("_", (R, "finDerechaS1"))
            ]),
            ("finDerechaS2", [
                ("#", (R, "vuelvoUnoSigma2")),
                ("_", (R, "finDerechaS2"))
            ]),
            ("vuelvoUnoSigma1", [
                ("#", (L, "graboSigma1"))
            ]),
            ("vuelvoUnoSigma2", [
                ("#", (L, "graboSigma2"))
            ]),
            ("graboSigma1", [
                ("#", (W sigma1, "vuelvoCentroIzqS1")) 
            ]),
            ("graboSigma2", [
                ("#", (W sigma2, "vuelvoCentroIzqS2")) 
            ]),
            ("vuelvoCentroIzqS1", [
                ("#", (R, "busco@S1")),
                ("_", (L, "vuelvoCentroIzqS1"))
            ]),
            ("vuelvoCentroIzqS2", [
                ("#", (R, "busco@S2")),
                ("_", (L, "vuelvoCentroIzqS2"))
            ]),
            ("busco@S1", [
                ("@", (W sigma1, "muevoIzquierda")),
                ("_", (L, "busco@S1"))
            ]),
            ("busco@S2", [
                ("@", (W sigma2, "muevoIzquierda")),
                ("_", (L, "busco@S2"))
            ]),
            ("muevoIzquierda", [
                ("_", (L, "decidoSigma"))
            ]),
            ("decidoSigma", [
                ("#", (R, "vuelvoCentroFinal")),
                (sigma1, (W "@", "vuelvoCentroDerechaS1")),
                (sigma2, (W "@", "vuelvoCentroDerechaS2"))
            ]),
            ("vuelvoCentroFinal", [
                ("#", (W "#", "centroFinal")),
                ("_", (R, "vuelvoCentroFinal"))
            ]),
            ("centroFinal", [
                ("#", (W "#", "h"))
            ])
        ]

cintaReverse :: Cinta
cintaReverse = ([sigma1, sigma2, sigma1, sigma1],"#",[])

ejecutarReverse = ejecucion reverseMT cintaReverse
