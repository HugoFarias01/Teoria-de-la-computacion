-- Tarea 3 - Hugo Farías (258218)

module Tarea3 where

-- Ejercicio 1

type Simbolo = String
type Current = Simbolo

type Estado = String

type Cinta = ([Simbolo], Current, [Simbolo])

type Rama = (Simbolo, (Accion, Estado))

data Accion = Left
            | Right
            | Write Simbolo
            deriving (Show)

type Codigo = [(Estado, [Rama])]

-- Ejercicio 2

ultimo :: [a] -> a
ultimo (x:[]) = x
ultimo (x:xs) = ultimo xs

eliminarUltimo :: [a] -> [a]
eliminarUltimo (x:[]) = []
eliminarUltimo (x:xs) = x:eliminarUltimo xs

left :: Cinta -> Cinta
left (l, c, r) = ((eliminarUltimo l), (ultimo l), c:r)

right :: Cinta -> Cinta
right (l, c, (r:rs)) = (l ++ [c], r, rs) 

write :: Simbolo -> Cinta -> Cinta
write s (l, c, r) = (l, s, r)

dameRama :: Estado -> Codigo -> [Rama]
dameRama e ((e', r):rs)
    | e == e' = r
    | otherwise = dameRama e rs

dameAccion :: Simbolo -> [Rama] -> Accion
dameAccion s ((s', (a, e)):rs)
    | s == s' = a
    | otherwise = dameAccion s rs

--step :: Cinta -> Cinta
