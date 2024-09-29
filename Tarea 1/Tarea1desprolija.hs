-- Tarea 1 - Hugo Farías (258218)

module Tarea1desprolija where

import Tabla

-----------------
-- Ejercicio 1 --
-----------------

data E = Var X -- Variable
       | Constructor K [E] -- Constructores
       | Lambda X E -- Funciones unarias
       | Aplicacion E E -- Aplicaciones
       | Case E [Rama] -- Selector
       | Rec X E -- Recursión
       deriving (Show)

data Rama = Rama K ([X], E) -- Rama de case 
          deriving (Show)

type X = String -- Identificador de la variable
type K = String -- Identificador del constructor

-----------------
-- Ejercicio 2 --
-----------------

-- Valores
data V = ConstructorV X [V]
       | LambdaV X E
       deriving (Show)

-- Weak
data W = ConstructorW X [E]
       | LambdaW X E
       deriving (Show)

-----------------
-- Ejercicio 3 --
-----------------

type S = [(X, E)]

bajas :: S -> X -> S
bajas s x = del' s x

efecto :: E -> S -> E
efecto (Var x) s = case (lkup s x) of 
                        Just e -> e
                        Nothing -> Var x
efecto (Constructor k es) s = Constructor k (map ((flip efecto) s ) es)
efecto (Lambda x e) s = Lambda x (efecto e (bajas s x))
efecto (Aplicacion e1 e2) s = Aplicacion (efecto e1 s) (efecto e2 s)
efecto (Case e r) s = Case (efecto e s) (map (sustRama s) r)
efecto (Rec x e) s = Rec x (efecto e (bajas s x))

sustRama :: S -> Rama -> Rama
sustRama s (Rama k (x, e)) = Rama k (x, efecto e (borrarRamas s x))

borrarRamas :: S -> [X] -> S
borrarRamas s [] = s
borrarRamas s (x:xs) = borrarRamas (del' s x) xs 

-----------------
-- Ejercicio 4 --
-----------------

weak :: E -> W
weak (Constructor k es) = ConstructorW k es
weak (Lambda x e) = LambdaW x e
weak (Aplicacion e1 e2) = case (weak e1) of {
                        ConstructorW k es -> ConstructorW k (es ++ [e2]);
                        LambdaW x e3 -> weak (efecto e3 [(x, e2)]) 
                     }
weak (Case e r) = if (length xs == length es) then 
                     weak (efecto e1 (zip xs es)) 
                  else 
                     error "Distinto largo de las listas"
                  where (ConstructorW x es) = weak e
                        (xs, e1) = buscarRama r (ConstructorW x es)
weak (Rec x e) = weak (efecto e [(x, Rec x e)])

buscarRama :: [Rama] -> W -> ([X], E)
buscarRama [] (ConstructorW x es) = error "No existe caso"
buscarRama ((Rama k (xs,e)):rs) (ConstructorW x es)
 | k == x = (xs,e)
 | otherwise = buscarRama rs (ConstructorW x es)

-----------------
-- Ejercicio 5 --
-----------------

eval :: E -> V
eval e = case (weak e) of {
       (ConstructorW x es) -> ConstructorV x (map eval es);
       (LambdaW x e) -> LambdaV x e
}

-----------------
-- Ejercicio 6 --
-----------------

or2 = \b b2 -> case b of {
                     True -> True;
                     False -> b2
}

orchi :: E
orchi = Lambda "b1" (Lambda "b2" (Case (Var "b1") [
                                                 Rama "True" ([], (Constructor "True" [])),
                                                 Rama "False" ([], (Var "b2"))
                                                 ]))

test :: E
test = Aplicacion (Aplicacion (orchi) (Constructor "False" [])) (Constructor "True" [])

suma = \n n2 -> case n of {
                            0 -> n2;              
                            _ -> (suma (n-1) (n2+1))
}

sumachi :: E
sumachi = Rec "sumachi" (Lambda "n1" (Lambda "n2" (Case (Var "n1") [
                                                 Rama "O" ([], (Var "n2")),
                                                 Rama "S" (["z"], Constructor "S" [Aplicacion (Aplicacion (Var "sumachi") (Var "z")) (Var "n2")])
                                          ])))

tres :: E
tres = (Constructor "S" [Constructor "S" [Constructor "S" [Constructor "O" []]]])

seis :: E
seis = (Constructor "S" [Constructor "S" [Constructor "S" [Constructor "S" [Constructor "S"[Constructor "S"[Constructor "O" []]]]]]])

triple :: Int -> Int
triple = \n -> case n of
    0 -> 0
    _ -> 3 + triple (n - 1)

triplechi :: E 
triplechi = Rec "triplechi" (Lambda "n" (Case (Var "n") [
                                                        Rama "O" ([], (Constructor "O" [])),
                                                        Rama "S" (["z"], Aplicacion (Aplicacion (sumachi) tres) (Aplicacion (Var "triplechi") (Var "z")))
                                                 ]))

duplicar = \l -> case l of {
                            [] -> [];
                            (x:xs) -> x:x:(duplicar xs)
}

duplicarchi :: E
duplicarchi = Rec "duplicarchi" (Lambda "l" (Case (Var "l") [
                                        Rama "[]" ([], (Constructor "[]" [])),
                                        Rama ":" (["x", "xs"], (Constructor ":" [(Var "x"), (Constructor ":" [(Var "x"), (Aplicacion (Var "duplicarchi") (Var "xs"))])]))
                                   ]))

lista :: E
lista = Constructor ":" [(Constructor "S" [Constructor "O" []]), Constructor "[]" []]

lista2 :: E
lista2 = Constructor ":" [(Constructor "S" [Constructor "S" [Constructor "O" []]]), Constructor ":" [(Constructor "S" [Constructor "O" []]), Constructor "[]" []]] 

bool :: E
bool =  Aplicacion duplicarchi (Constructor ":" [Constructor "True" [], Constructor ":" [Constructor "False" [], Constructor "[]" []]])

ramaC :: E
ramaC = Rec "ramaC" (Lambda "arbol" (Case (Var "arbol") [
                                        Rama "Vacio" ([], (Constructor "[]" [])),
                                        Rama "Hoja" (["x"], (Constructor ":" [(Var "x"), (Constructor "[]" [])])),
                                        Rama "Nodo" (["x", "izq", "centro", "der"], (Constructor ":" [(Var "x"), Aplicacion (Var "ramaC") (Var "centro") ]))
                                   ]))

{- Árbol de prueba de la forma:
              1
            / | \ 
       Vacio  |  0
              3
            / | \             
           /  |  \
          0   4   2
-}
arbol :: E
arbol = Constructor "Nodo" [Constructor "S" [Constructor "O" []], Constructor "Vacio" [], Constructor "Nodo" [Constructor "S" [Constructor "S" [Constructor "S" [Constructor "O" []]]], Constructor "Hoja" [Constructor "O" []], Constructor "Hoja" [Constructor "S" [Constructor "S" [Constructor "S" [Constructor "S" [Constructor "O" []]]]]], Constructor "Hoja" [Constructor "S" [Constructor "S" [Constructor "O" []]]]], Constructor "Hoja" [Constructor "O" []]]
