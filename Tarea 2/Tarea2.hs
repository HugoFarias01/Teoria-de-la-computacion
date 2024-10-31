-- Tarea 2 - Hugo Farías (258218)

module Tarea2 where

import Tabla

-- Ejercicio 1
data P = Ass [X] [E] -- Asignación múltiple
       | Local [X] P -- Declaraciones de variables locales
       | Secuen P P -- Secuencia 
       | Case X [Rama] -- Selección
       | While X [Rama] -- Iteración
       deriving (Show)

data E = Cons C [E] -- Constructor
       | Var X -- Variable  
       deriving (Show)

data Rama = Rama (C, [X]) P -- Rama
          deriving (Show)

type X = String
type C = String

data V = ConstructorV C [V] -- Constructor 
       | Null -- Ausencia de valor
       deriving (Show)

-- Ejercicio 2
type M = [(X, V)]

busqueda :: M -> X -> V
busqueda m x = case (lkup m x) of
                                Just v -> v
                                Nothing -> error "No se encuentra en memoria"

actualizar :: M -> M -> M
actualizar m [] = m
actualizar m ((x,v):xs) = actualizar (upd m (x,v)) xs

alta :: M -> [X] -> M
alta m [] = m
alta m (x:xs) = alta ((x, Null):m) xs

bajas :: M -> [X] -> M
bajas m [] = m
bajas m (x:xs) = bajas (del m x) xs

-- Ejercicio 3
eval :: E -> M -> V
eval (Cons c es) m = ConstructorV c (map (\e -> eval e m) es)
eval (Var x) m = busqueda m x

-- Ejercicio 4
ejecutar :: P -> M -> M
ejecutar (Ass xs es) m = actualizar m (zip xs (map (\e -> eval e m) es))
ejecutar (Local xs p) m = let m' = ejecutar p (alta m xs) in bajas m' xs
ejecutar (Secuen p1 p2) m = let m' = ejecutar p1 m in ejecutar p2 m'
ejecutar (Case x ramas) m = case busqueda m x of
                                   ConstructorV c vs -> 
                                          case buscarRama c ramas of
                                                 Just (xs, p) -> 
                                                        let prog = Local xs (Secuen (Ass xs (map convertir vs)) p) 
                                                        in if (length(xs) == length(vs)) then 
                                                               ejecutar prog m 
                                                           else 
                                                               error "Distinto largo"                                                 
                                                 Nothing -> error "No existe la rama con ese constructor"
                                   Null -> error "No es posible una rama con Null"
ejecutar (While x ramas) m = case busqueda m x of 
                                   ConstructorV c vs ->
                                          case buscarRama c ramas of 
                                                 Just (xs, p) ->
                                                        if (length(xs) == length(vs)) then
                                                               let prog = Local xs (Secuen (Ass xs (map convertir vs)) p)
                                                                   m' = (ejecutar prog m)
                                                               in ejecutar (While x ramas) m'
                                                        else 
                                                               error "Distinto largo"
                                                 Nothing -> m

convertir :: V -> E
convertir (ConstructorV c vs) = Cons c (map convertir vs)
convertir Null = error "No se puede convertir Null a E"                                   

buscarRama :: C -> [Rama] -> Maybe ([X], P)
buscarRama _ [] = Nothing
buscarRama c (Rama (cond, xs) p : ramas)
    | c == cond = Just (xs, p)
    | otherwise = buscarRama c ramas

-- Ejercicio 5
par :: P
par = Local ["n'"] (
              Secuen (Ass ["n'", "result"] [Var "n", Cons "True" []]) 
                     (While "n'" [
                            Rama ("S", ["x"]) (
                                   Secuen (Case "result" [
                                                 Rama ("True", []) (Ass ["result"] [Cons "False" []]),
                                                 Rama ("False", []) (Ass ["result"] [Cons "True" []])
                                                 ]
                                          )
                                          (Ass ["n'"] [Var "x"])
                                   )       
                            ]      
                     )
              )

suma :: P
suma = Local ["n'"] (
              Secuen (Ass ["n'", "result"] [Var "n", Var "m"]) 
                     (While "n'" [
                            Rama ("S", ["x"]) (Ass ["n'", "result"] [Var "x", Cons "S" [Var "result"]])
                            ]
                     )
              ) 

largo :: P
largo = Local ["l'"] (
              Secuen (Ass ["l'", "result"] [Var "l", Cons "O" []]) 
                     (While "l'" [
                            Rama (":", ["x","xs"]) (Ass ["l'", "result"] [Var "xs", Cons "S" [Var "result"]])
                            ]
                     )
              )

igualdadN :: P
igualdadN = Local ["m'", "n'", "resultM"] (
              Secuen (Ass ["m'", "n'", "resultM", "result"] [Var "m", Var "n", Cons "False" [], Cons "True" []]) (
                     Secuen (esMayor) 
                            (Case "resultM" [
                                   Rama ("True", []) (
                                          (While "m'" [
                                                 Rama ("S", ["x"]) (
                                                        Case "n'" [
                                                               Rama ("S", ["y"]) (Ass ["m'", "n'"] [Var "x", Var "y"]),
                                                               Rama ("O", []) (Ass ["result", "m'"] [Cons "False" [], Cons "O" []])
                                                        ]
                                                 )
                                          ])
                                   ),
                                   Rama ("False", []) (
                                          (While "n'" [
                                                 Rama ("S", ["x"]) (
                                                        Case "m'" [
                                                               Rama ("S", ["y"]) (Ass ["n'", "m'"] [Var "x", Var "y"]),
                                                               Rama ("O", []) (Ass ["result", "n'"] [Cons "False" [], Cons "O" []])
                                                        ]
                                                 )
                                          ])
                                   )]
                            )
                     )
              )      

esMayor :: P
esMayor = Local ["a'", "n'"] (
              Secuen (Ass ["a'", "b'", "resultM"] [Var "m", Var "n", Cons "False" []]) 
                     (While "a'" [
                            Rama ("S", ["x"]) (
                                   Case "b'" [
                                          Rama ("S", ["y"]) (Ass ["a'", "b'"] [Var "x", Var "y"]),
                                          Rama ("O", []) (Ass ["resultM", "a'"] [Cons "True" [], Cons "O" []])
                                   ]
                            )
                     ])
              )

concatenar :: P
concatenar = Local ["l1'", "l2'"] (
              Secuen (Ass ["l1'", "l2'", "result"] [Var "l1", Var "l2", Cons "[]" []]) (
                     Secuen (
                            Secuen (
                                   While "l1'" [
                                          Rama (":", ["x", "xs"]) (
                                                 Ass ["result", "l1'"] [Cons ":" [Var "x", Var "result"], Var "xs"]
                                          ) 
                                   ]) (
                                   While "l2'" [
                                          Rama (":", ["x", "xs"]) (
                                                 Ass ["result", "l2'"] [Cons ":" [Var "x", Var "result"], Var "xs"]
                                          )
                                   ])
                            ) (invertirLista)
                     )
              )

invertirLista :: P
invertirLista = Local ["aux", "temp"] (
             Secuen (Ass ["aux", "temp"] [Cons "[]" [], Var "result"]) 
                    (Secuen 
                        (While "temp" [
                            Rama (":", ["x", "xs"]) (
                                Secuen
                                    (Ass ["aux"] [Cons ":" [Var "x", Var "aux"]])
                                    (Ass ["temp"] [Var "xs"])
                            )
                        ])
                        (Ass ["result"] [Var "aux"])
                    )
           )

-- PRUEBAS

-- Defino una memoria con datos que voy cambiando para hacer las pruebas en cada programa
m :: M
m = [
    ("n", construirNumero 2),
    ("m", construirNumero 1),
    ("l", construirLista [1, 2]),
    ("l1", construirLista [3, 2, 7]),
    ("l2", construirLista [4, 5]),
    ("result", ConstructorV "False" [])]

-- Función que construye un número a partir de un entero
construirNumero :: Int -> V
construirNumero 0 = ConstructorV "O" []
construirNumero n = ConstructorV "S" [construirNumero (n - 1)]

-- Función que hace lo opuesto, descontruye un número pasandolo a entero
desconstruirNumero :: V -> Int
desconstruirNumero (ConstructorV "O" []) = 0
desconstruirNumero (ConstructorV "S" [v]) = 1 + desconstruirNumero v
desconstruirNumero _ = error "Formato de número no válido"

-- Función que construye una lista con ":" y "[]" a partir de una lista de enteros
construirLista :: [Int] -> V
construirLista [] = ConstructorV "[]" []
construirLista (x:xs) = ConstructorV ":" [ConstructorV (show x) [], construirLista xs]

-- Función que hace lo opuesto, descontruye una lista pasandola a una lista de enteros
desconstruirLista :: V -> [Int]
desconstruirLista (ConstructorV "[]" []) = []
desconstruirLista (ConstructorV ":" [ConstructorV x [], xs]) = 
    read x : desconstruirLista xs
desconstruirLista _ = error "Formato de lista no válido"

-- Ejecutadores de cada prueba
ejecutarPar :: V
ejecutarPar = busqueda (ejecutar par m) "result"

ejecutarSuma :: V
ejecutarSuma = busqueda (ejecutar suma m) "result"

ejecutarLargo :: Int 
ejecutarLargo = desconstruirNumero (busqueda (ejecutar largo m) "result")

ejecutarIgualdadN :: V
ejecutarIgualdadN = busqueda (ejecutar igualdadN m) "result"

ejecutarConcatenar :: [Int]
ejecutarConcatenar = desconstruirLista (busqueda (ejecutar concatenar m) "result")