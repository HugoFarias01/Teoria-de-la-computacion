module SolucionTarea0 where

------------
--SOLUCIÓN--
------------

--Ejercicio 1
data E = 
        Var X
    |   Empty
    |   Unit Z
    |   Pert Z E
    |   Union E E
    |   Intersection E E
    |   Difference E E
    |   Included E E
    |   Assign X E

type X = String
type Z = Int

--Ejercicio 2
data V =
        C [Z]
    |   B Bool
    deriving (Show)

--Ejercicio 3
type M = [(X,V)]

lkup :: X -> M -> V
lkup x [] = error "Variable not defined."
lkup x ((x',v):m')
    | x == x' = v
    | otherwise = lkup x m'

upd :: M -> (X,V) -> M
upd [] xv = [xv]
upd (xv@(x,v):m') xv'@(x',v')
    | x == x' = xv':m'
    | otherwise = xv:(upd m' xv')

--Ejercicio 4
belongs :: Int -> [Int] -> Bool
belongs x c = elem x c

union :: [Int] -> [Int] -> [Int]
union c1 c2 = nub (c1 ++ c2)

nub :: [Int] -> [Int]
nub [] = []
nub (x:xs)
    | elem x xs = nub xs
    | otherwise = x:(nub xs)

intersection :: [Int] -> [Int] -> [Int]
intersection [] c2 = []
intersection (x:xs) c2
    | elem x c2 = x:(intersection xs c2)
    | otherwise = intersection xs c2

difference :: [Int] -> [Int] -> [Int]
difference [] c2 = []
difference (x:xs) c2
    | not (elem x c2) = x:(difference xs c2)
    | otherwise = difference xs c2

included :: [Int] -> [Int] -> Bool
included [] c2 = True
included (x:xs) c2 = (elem x c2) && (included xs c2)

eval :: (M,E) -> (M,V)
eval (m, Var x) = (m, lkup x m)
eval (m, Empty) = (m, C [])
eval (m, Unit z) = (m,C [z])
eval (m, Pert z e) = case eval (m,e) of
    (m', C c) -> (m', B (belongs z c))
eval (m, Union e1 e2) = case eval (m,e1) of
    (m', C c1) -> case eval (m',e2) of
        (m'', C c2) -> (m'', C (union c1 c2))
eval (m, Intersection e1 e2) = case eval (m,e1) of
    (m', C c1) -> case eval (m',e2) of
        (m'', C c2) -> (m'', C (intersection c1 c2))
eval (m, Difference e1 e2) = case eval (m,e1) of
    (m', C c1) -> case eval (m',e2) of
        (m'', C c2) -> (m'', C (difference c1 c2))
eval (m, Included e1 e2) =  case eval (m,e1) of
    (m', C c1) -> case eval (m',e2) of
        (m'', C c2) -> (m'', B (included c1 c2))
eval (m, Assign x e) =  case eval (m,e) of
    (m', v) -> (upd m' (x,v), v)

--Ejercicio 5
conj1 :: E
conj1 = Union (Union (Unit 1) (Unit 2)) (Unit 3)

conj2 :: E
conj2 = Union (Union (Unit 2) (Unit 3)) (Unit 4)

conj3 :: E
conj3 = Union conj1 conj2

conj4 :: E
conj4 = Intersection conj1 conj2

pert1 :: E
pert1 = Pert 2 conj1

pert2 :: E
pert2 = Pert 3 conj4

incl1 :: E
incl1 = Included conj1 conj2

incl2 :: E
incl2 = Included conj4 conj2

incl3 :: E
incl3 = Included conj1 conj3

ass1 :: E
ass1 = Assign "w" conj1

ass2 :: E
ass2 = Assign "x" conj4

ass3 :: E
ass3 = Assign "y" pert2

ass4 :: E
ass4 = Assign "z" incl2

-------
--FIN--
-------