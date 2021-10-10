-- 1) define usando foldl o foldr usando lambda-expresiones
last' :: [a] -> a
last' = foldl (\_ x -> x) undefined
reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x:acc) [] xs
all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr (\x acc -> if f x then True && acc else False) True xs
minimum' :: (Ord a) => [a] -> a
minimum' (x:xs) = foldl (\min x -> if x < min then x else min) x xs
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' p = foldr (\x xs -> if p x then x:xs else []) []

-- 2) variantes foldr y foldrl

-- 3)define usando listas intensionales / funcion orden superior
-- a) [1, −1, 2, −2, 3, −3, 4, −4, . . .].
tresA = concat [[x , -x] | x <- [1..]]
-- b)Una lista infinita [(0, 0),(0, 1),(1, 0),(0, 2),(1, 1),(2, 0),(0, 3),(1, 2), . . .], que sirva
-- como enumeraci´on de todas las parejas de n´umeros naturales.
tresB = concat [[(x,y) | x <- [0..n], y <-[0..x], x+y == n] | n <- [0..]]
lista_1b = concat [[(x,y) | x <- [0..n], y <- [0..n], x + y == n] | n <- [1..]]

-- 4)
-- a) sufijos xs = lista de todos los sufijos de xs
sufijos xs = [drop x xs | x <- [0.. length xs]]