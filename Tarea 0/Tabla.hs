-- insertar, eliminar , actualizar, recuperar, crear
module Tabla(crear,lkup,upd,del) where
type Tabla a b = [(a,b)]

crear :: Tabla a b
crear = []

lkup :: Eq a => Tabla a b -> a -> b
lkup [] a = error "variable no definida"
lkup ((x,y):xys) a
 | x == a = y
 | otherwise = lkup xys a
 
upd :: Eq a => Tabla a b -> (a,b) -> Tabla a b
upd [] p = [p] --upd [] (k,v) = [(k,v)]
upd ((x,y):xys) (k,v)
 | x == k = (x,v):xys
 | otherwise = (x,y):(upd xys (k,v))

upd' :: Eq a => Tabla a b -> (a,b) -> Tabla a b
upd' t p = p:t
 

del :: Eq a => Tabla a b -> a -> Tabla a b
del [] a = []
del ((x,y):xys) a
 |x == a = xys
 |otherwise = (x,y):(del xys a)
 
del' :: Eq a => Tabla a b -> a -> Tabla a b
del' t k = filter (\(x,y)-> x /= k) t
