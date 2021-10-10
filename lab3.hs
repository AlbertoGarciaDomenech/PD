--Alberto Garcia Domenech

--1)1. Define mediante foldr o foldl, en lugar de mediante recursi´on expl´ıcita, las siguientes
--funciones predefinidas en Prelude. Expresa mediante λ-expresiones el primer argumento
--de la la funci´on fold que utilices.

--a)last
last' :: [a] -> a
last' = foldl (\_ x -> x) undefined
--b)reverse
reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x : acc) [] xs
--c)all
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = foldr (\acc x -> if p acc then True && x else False) True xs
--d)minimum
minimum' :: (Ord a) => [a] -> a
minimum' (x:xs) = foldl (\min x -> if x < min then x else min) x xs
--e)map
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []
--f)filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []
--g)takeWhile
takeWhile' :: (a -> Bool) -> [a] ->  [a]
takeWhile' p = foldr (\x xs -> if p x then x:xs else []) []

--2)2. Programa, las siguientes variantes de foldl y foldr, que operan con listas no vac´ıas y
--no usan valor acumulado inicial:

--a)• foldr1 L [x1, . . . , xn] = x1 Lx2L. . .Lxn (con L asociando por la derecha)
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f (x:xs) = foldr f x xs 
--b)foldl1 L [x1, . . . , xn] = x1 Lx2 L. . .Lxn (con L asociando por la izquierda)
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x:xs) = foldl f x xs 
 
--3)Define expresiones Haskell usando funciones de orden superior predefinidas de Haskell
--y/o listas intensionales para representar

--a) La lista [1, −1, 2, −2, 3, −3, 4, −4, . . .].
listaAlternos :: [Integer]
listaAlternos = concat [[x,-x]| x <- [1..]]
--b)Una lista infinita [(0, 0),(0, 1),(1, 0),(0, 2),(1, 1),(2, 0),(0, 3),(1, 2), . . .], que sirva
--como enumeraci´on de todas las parejas de n´umeros naturales.
listaParejas :: [(Integer,Integer)]
listaParejas = concat [[(x,y) | x <- [0..n], y <- [0..n], x+y == n] | n <- [0..]]

--4)Programa las siguientes funciones, usando orden superior o listas intensionales.

--a)sufijos xs = lista de todos los sufijos de xs
--sufijos [1,2,3] = [[1,2,3],[2,3],[3],[]]
sufijos :: [a] -> [[a]]
sufijos xs = [drop x xs | x <- [0.. length xs]]
--b)sublistas xs = lista de todas las sublistas de xs
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : filter (/= x) (removeDuplicates xs)
sublistas :: (Ord a) => [a] -> [[a]]
sublistas xs = removeDuplicates [take n x | n <- [0..length xs], x <- sufijos xs]
--c)permutaciones xs = lista de todas las permutaciones de xs
--permutaciones [1,2,3]=[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
permutaciones :: (Eq a) => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones l = [a:x | a <- l, x <- (permutaciones $ filter (\x -> x /= a) l)]
--d)sumandos n devuelve la lista de todas las descomposiciones en sumandos positivos de n
--sumandos 3 = [[1,1,1],[1,2],[3]].
