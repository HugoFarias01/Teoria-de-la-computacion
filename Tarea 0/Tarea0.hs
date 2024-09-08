import Tabla

data E = Var X
       | Vacio
       | Uno Z
       | Pert E E
       | Union E E
       | Inter E E
       | Difer E E
       | Incl E E
       | Ass X E
       deriving Show

type X = String
type Z = Int

data V = Conjunto [Int] 
       | B Bool
       deriving Show

type M = [(X, V)]

belongs :: Int -> [Int] -> Bool
belongs e [] = False
belongs e (x:xs) 
  | x == e = True 
  | otherwise = belongs e xs

union :: [Int] -> [Int] -> [Int]
union a [] = a
union [] b = b
union (a:as) (b:bs)
  | a == b = a:(union as bs)
  | a > b = b:(union (a:as) bs)
  | a < b = a:(union as (b:bs))

intersection :: [Int] -> [Int] -> [Int]
intersection a [] = []
intersection [] b = []
intersection (a:as) (b:bs)
  | a == b = a:(intersection as bs)
  | a > b = (intersection (a:as) bs)
  | a < b = (intersection as (b:bs))

difference :: [Int] -> [Int] -> [Int]
difference a [] = a
difference [] b = []
difference (a:as) (b:bs)
  | a == b = difference as bs
  | a > b = difference (a:as) bs
  | a < b = a:(difference as (b:bs))

included :: [Int] -> [Int] -> Bool
included [] b = True
included a [] = False
included (a:as) (b:bs)
  | a == b = included as bs
  | a > b = included (a:as) bs
  | a < b = False

eval :: (M, E) -> (M, V)
eval (m, Var x) = (m, lkup m x)
eval (m, Vacio) = (m, Conjunto [])
eval (m, Uno z) = (m, Conjunto [z])

ejemploM :: M
ejemploM = [("a", Conjunto [1, 2, 3]), ("b", Conjunto [4, 5]), ("c", B True), ("d", B False)]

conj1 :: E
conj1 = Union (Uno 1) (Union (Uno 2) (Uno 3))

conj2 :: E
conj2 = Union (Uno 2) (Union (Uno 3) (Uno 4))

conj3 :: E
conj3 = Union conj1 conj2

conj4 :: E
conj4 = Inter conj1 conj2

pert1 :: E
pert1 = Pert (Uno 2) conj1

pert2 :: E
pert2 = Pert (Uno 3) conj4

incl1 :: E
incl1 = Incl conj1 conj2

incl2 :: E
incl2 = Incl conj4 conj2

incl3 :: E
incl3 = Incl conj1 conj3

ass1 :: E
ass1 = Ass "w" conj1

ass2 :: E
ass2 = Ass "x" conj4

ass3 :: E
ass3 = Ass "y" pert2

ass4 :: E 
ass4 = Ass "z" incl2