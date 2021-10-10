-- Alberto Garcia Domenech
-- 1)
cuadrado1 :: (Integral a) => a ->[a]
cuadrado1 n
    |n == 0 = [0]
    |otherwise = cuadrado1 (n-1) ++ [n^2]
cuadrado2 :: (Integral a) => a ->[(a,a)]
cuadrado2 n
    |n == 0 = [(0,0)]
    |otherwise = [(n, n^2)] ++ cuadrado2 (n-1)
sumCos :: (Eq a, Floating a) => a -> a
sumCos n
    |n == 1 = abs (cos n)
    |otherwise = sumCos (n -1) + (n * abs (cos n))
sumMenores :: (Integral a) => a -> a
sumMenores n
    |(n < 5) || (n == 5) = 0 -- No sumamos ni el 3 ni el 5 
    |otherwise = let x = if ((mod n 5 == 0) || (mod n 3 == 0)) then n else 0 in x + sumMenores(n-1)

-- 2)
cuadrado :: Integral a => a -> a
cuadrado n = n^2
cuadrado3 :: Integral a => a -> [a]
cuadrado3 n = map cuadrado [0..n]

cuadrado' :: Integral a => a -> (a,a)
cuadrado' n = (n,n^2)
cuadrado4 :: Integral a => a -> [(a,a)]
cuadrado4 n = map cuadrado' [n,n-1..0]

sumCos' :: (Enum a, Floating a) => a -> a
sumCos' n = sum [i * abs (cos i) | i <- [1..n]]

-- 3)  
-- f x = g x, para todo n ≤ x ≤ m
iguales :: (Enum a, Eq b) => (a -> b) -> (a -> b) -> a -> a -> Bool
iguales f g n m = foldr (&&) True ((zipWith (\x y -> x == y) (map (f) [n..m]) (map (g) [n..m])))

-- menor x con n ≤ x ≤ m que verifica p
menorA :: (Enum a) => a -> a -> (a -> Bool) -> a
menorA n m p = head [i |i <- [n..m],(p i == True)]
-- mayor x ≤ n que verifica p
mayor :: (Num a, Enum a) => a -> (a -> Bool) -> a
mayor n p = head [ i | i <- [n, n-1..0],(p i == True)]

-- existe x con n ≤ x ≤ m que verifica p
ex :: (Enum a) => a -> a -> (a->Bool)-> Bool
ex n m p = elem True (map p [n..m])

-- 4)
-- filter2 xs p q = (us, vs) donde us son los elementos de xs que cumplen p y vs los que cumplen q
filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a],[a])
filter2 xs p q = (filter p xs, filter q xs)

-- filters xs ps = [xs1, . . . , xsn], donde xsi son los elementos de xs que cumplen pi, supuesto que ps es [p1, . . . , pn]
filters :: [a] -> [a -> Bool] -> [[a]]
filters xs [] = []
filters xs ps = filter (head ps) xs : filters xs (tail ps)