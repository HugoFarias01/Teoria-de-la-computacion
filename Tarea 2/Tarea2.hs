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
