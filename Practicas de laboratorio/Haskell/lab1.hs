-- Alberto Garcia Domenech

-- Ej 1
-- a)
years = 10^10 / (60*60*24*365)

-- b)

intyears = truncate years
x :: Integer
x = intyears * (60*60*24*365)
intdays = div (10^10 - x) (24*60*60)
y :: Integer
y = intdays * (60*60*24) + x
inthours = div (10^10 - y) (60*60)
z :: Integer
z = inthours * (60 * 60) + y
intmins = div (10^10 - z) (60)
w :: Integer
w = intmins * 60  + z
intsegs = (10^10 - w) 

totalRestantes = (intyears, intdays, inthours, intmins, intsegs)

-- c)
fyears :: Fractional a => a -> a
fyears n = n / (60*60*24*365)

-- frestantes x = let
--     years2 = truncate (x / (60*60*24*365)ñ)
--     days2 = let aux1 = years2 * (60*60*24*365) in div (x - aux1) (24*60*60)
--     hours2 = let aux2 = (years2*365 + days2) * (60*60*24) in div (x-aux2) (60*60)
--     mins2 = let aux3 = (years2*365 + days2*24 + hours2) * (60*60) in div (x - aux3) (60)
--     segs2 = let aux4 = (years2*365 + days2*24 + hours2*60 + mins2) * (60) in (x - aux4)
--     in
--         (days2,hours2,mins2,segs2)


-- Ej 2
-- Poco (last [1..10^5])
-- Poco (last [1..10^7])
--Mucho(last[1..10^20])
--Poco(head[1..10^20])
-- *** Exception: Prelude.last: empty list (last[10^20..1])
--Poco(head(tail[1..10^20]))
--Mucho(length [1..10^20])
--Poco(last (take (10^7) [1..10^20]))
--Poco(head (take (10^7) ([1..100] ++ [1..10^20])))
--Poco(last (take 100 ([1..10^20] ++ [1..100])))
--Mucho (last (drop 100 ([1..10^20] ++ [1..100])))
--Poco (head (drop (10^7) ([1..10^20] ++ [1..100])))
--Regular ([1..10^7]==[1..10^7])
--Mucho ([1..10^20]==[1..10^20])
--Mucho ([1..10^20]==[1..10^20+1])
--Poco ([1..10^20]==[2..10^20])
--Regular (head (reverse [1..10^7]))
--Regular (last (reverse [1..10^7]))
--Mucho (reverse [1..10^20] == reverse [1..10^20+1])

--Ej 3
media xs = sum xs / fromIntegral(length xs)

--Ej 4
digitos :: Integral a => a -> a
digitos x
    |x < 10  = 1
    |otherwise = 1 + digitos (div x 10)

reduccion :: Integral a => a -> a
reduccion x
    |x < 0  = (-x)
    |x < 10  = x
    |otherwise = reduccion (mod x 10 + div x 10)

perm :: Integral a => a -> a
perm n
    |n == 0   = 1
    |otherwise = n * perm(n-1)

var :: Integral a => a -> a -> a 
var n m = div (perm n) (perm (n-m))

comb :: Integral a => a -> a -> a
comb n m = div (perm n) ((perm m) * (perm(n-m)))

--Ej 5

conj :: Bool -> Bool -> Bool
conj False _ = False
conj _ False = False
conj True True = True
conj1 :: Bool -> Bool ->  Bool
conj1 True x = x
conj1 x True = x
