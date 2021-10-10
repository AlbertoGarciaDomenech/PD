-- PRACTICA FINAL PROGRAMACION DECLARATIVA 2020-2021
-- ALBERTO GARCIA DOMENECH
data Rel a = R [(a,a)] deriving (Read, Show)

instance (Eq a) => Eq (Rel a) where
  (==) (R x) (R y) = (&&) (foldl (&&) True [elem a y | a <- x]) (foldl (&&) True[elem a x | a <- y])



r0 = R [('D','8'),('E','B'),('C','B'),('E','C'),('8','D')]
r0'= R [([1,2],[]),([2,-2],[3,3,1]),([1,3],[0]),([4],[4])]
r1 = R [(1,1),(5,2),(7,3)]
r2 = R [("1","a"),("2","b"),("3","c"),("4", "d"),("5","e")]
r3 = R [(1,1),(2,2),(3,3)]  --relacion eq
r4 = R [(1,1),(1,2),(1,3),(2,2),(2,1),(2,3), (3,1),(3,2),(3,3), (4,4),(4,5),(4,6),(5,4),(5,5),(5,6),(6,4),(6,5),(6,6)] --relacion eq
r5 = R [(1,2),(2,4),(4,8),(8,16)] -- r5 y r6 son iguales. {xRy si y es el doble de x}
r6 = R [(4,8),(1,2),(8,16),(2,4)] -- r5 y r6 son iguales. {xRy si y es el doble de x}

esRelacion :: (Eq a) => Rel a -> Bool
esRelacion (R x) = not (duplicados x)
duplicados :: (Eq a) => [a] -> Bool
duplicados [] = False
duplicados (x:xs) = (elem x xs) || (duplicados xs)

--quita duplicados de una lista
quitardup :: (Eq a) => [a] -> [a]
quitardup l = quitardup' l []
  where
    quitardup' [] _ = []
    quitardup' (x:xs) ls
        | elem x ls = quitardup' xs ls
        | otherwise = x : quitardup' xs (x:ls)

dominio :: (Eq a) => Rel a -> [a]
dominio (R x) = if esRelacion (R x) then quitardup((dom x)) else []
dom :: [(a,a)] -> [a]
dom [] = []
dom (x:xs) = (fst x) : dom xs

rango :: (Eq a) => Rel a -> [a]
rango (R x) = if esRelacion (R x) then quitardup((ran x)) else []
ran :: [(a,a)] -> [a]
ran [] = []
ran (x:xs) = (snd x) : ran xs

soporte :: (Eq a) => Rel a -> [a] --soporte igual a union entre dominio y rango
soporte (R x) = if esRelacion (R x) then quitardup((sop x)) else []
sop :: [(a,a)] -> [a]
sop [] = []
sop (x:xs) = (fst x) : (snd x): sop xs

-- relacion equiv = reflex,simet y transitiva
relEquivalencia :: (Eq a) => Rel a -> Bool
relEquivalencia r =  esRelacion r && relReflex r && relSimet r && relTrans r
--tanto para comprobar si son relaciones reflexivas,simetricas o transitivas asummimos que son relaciones binarias
--reflexiva =  si para todo a dentro del dominio de R se cumple aRa
relReflex :: (Eq a) => Rel a -> Bool
relReflex (R x) = foldl (&&) True [elem (a,a) x | a <- dominio (R x) ]
-- simetrica = si para todo x,y dentro del conjunto soporte de R, si se cumple xRy se cumple yRx
relSimet :: (Eq a) => Rel a -> Bool
relSimet (R x) = foldl (&&) True [ elem (snd (a), fst (a)) x | a <- x]
--transitiva = si para todo x,y,z dentro del conjunto soporte de R, si se cumple xRy y yRz entonces se cumple xRz
relTrans :: (Eq a) => Rel a -> Bool
relTrans (R x) = foldl (&&) True [elem (fst c, b) x | c <- x, b <- rango (R x), elem (snd(c),b) x]

conjCociente :: (Eq a) => Rel a -> [[a]]
conjCociente r = if relEquivalencia r then clasesEq r else error "No es una relacion de equivalencia."
clasesEq ::  (Eq a) => Rel a -> [[a]]
clasesEq (R x) = [ [y | y <- rango (R x), elem (a,y) x] | a <- dominio (R x)]

-- generaDiv n m = r donde r es la relacion {(x, y) | n ≤ x, y ≤ m, x es divisor de y}.
generaDiv :: (Eq a, Integral a) => a -> a -> Rel a
generaDiv n m = R [(x,y) | x <- [n..m], y <- [x..m], (mod y x) == 0]

-- generaGE xs = r donde r es la relacion ≥ sobre el conjunto de elementos de la lista xs
generaGE :: (Eq a, Ord a) => [a] -> Rel a
generaGE xs = R (quitardup (concat [ [(y , a) | a <- xs, y >=a ] | y <- xs]))

--compSop comprueba q las dos relaciones tienen el mismo soporte (estan definidas en un mismo conjunto)
compSop :: (Eq a) => Rel a -> Rel a -> Bool
compSop (R x) (R y) = (&&) (foldl (&&) True [elem a (soporte (R y)) | a <- soporte (R x)]) (foldl (&&) True [elem a (soporte (R x)) | a <- soporte (R y)])

-- composicion r1 r2 = r1 ◦ r2 = r2(r1)
composicion :: (Eq a) => Rel a -> Rel a -> Rel a
composicion (R x) (R y) = if (compSop (R x) (R y)) then compos (R x) (R y) else error "No estan definidos sobre el mismo conjunto"
compos :: (Eq a) => Rel a -> Rel a -> Rel a
compos (R a) (R b) = R [ (x,y) | (x,u) <- a, (y,v) <- b, u == v]


-- ENTRADA SALIDA I/O

muestraCol :: (Eq a, Show a) => [a] -> IO String
muestraCol a = return (concat (concat [(show x) : "\t"  :[]| x <- a] ))
muestraFil :: (Eq a, Show a) => Rel a -> IO String
muestraFil (R l) = do
                     return (concat [(show x) ++ "\t|" ++ (concat [if (elem (x,y) l) then " x\t" else "\t" | y <- soporte (R l)]) ++ "|\n" | x <- soporte (R l)])

muestraRel :: (Show a, Eq a) => Rel a -> IO()                  
muestraRel (R a) = do
               x <- muestraCol (soporte (R a))
               y <- muestraFil (R a)
               putStr ("\t " ++ (x) ++ "\n")
               putStr (y)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

--mostrar relacion con el soporte ordenado
muestraColOrd :: (Ord a, Show a) => [a] -> IO String
muestraColOrd a = return (concat (concat [(show x) : "\t"  :[]| x <- quicksort a] ))
muestraFilOrd :: (Ord a, Show a) => Rel a -> IO String
muestraFilOrd (R l) = do
                         return (concat [(show x) ++ "\t|" ++ (concat [if (elem (x,y) l) then " x\t" else "\t" | y <- quicksort (soporte (R l))]) ++ "|\n" | x <- quicksort (soporte (R l))])
muestraRelOrd ::  (Ord a, Show a) => Rel a -> IO ()                
muestraRelOrd (R a) = do
               x <- muestraColOrd (soporte (R a))
               y <- muestraFilOrd (R a)
               putStr ("\t " ++ (x) ++ "\n")
               putStr (y)

--funcion filtro para terminar el input del usuario
acaba :: String -> Bool
acaba x
 |x == "FIN" = True
 |otherwise = False
-- pide al usuario que introduzca input (getLine) hasta que introduzca el input que determine el fin (f x == True)
inputHastaAcabar:: (String -> Bool) -> IO [String]
inputHastaAcabar f = do
                 x <- getLine
                 if f x then return [] else (x:) <$> inputHastaAcabar f

introRel :: IO (Rel [Char] )
introRel = do
             putStr("Introduce una relacion binaria en la forma de los pares que conforman la relacion. Introduce uno a uno los pares y cuando quieras finalizar escribe FIN\n")
             line <- inputHastaAcabar acaba
             line <- (formatRelacion line)
             return (line)

--recibimos lista de string ["(x,y)","(z,w)"] y devolvemos Relacion R [(x,y), (z,w)]
formatRelacion :: [String] -> IO (Rel [Char])
formatRelacion line = return ( R ([getPar x | x <- line]))
--recibimos string "(x,y)" devolver tupla (x,y)
getPar :: [Char] -> ([Char],[Char])
getPar str = let {x = (getX (drop 1 str) ','); y = init (listDifference str ("("++x++",")) } in (x,y) 
--recibimos "x,y)" devolver el primer item de la tupla
getX :: [Char] -> Char -> [Char]
getX str c
 |str !! 0 == '[' = "[" ++ (getX (drop 1 str)  ']') ++ "]"
 |str !! 0 == '(' = "(" ++ (getX (drop 1 str)  ')') ++ ")"
 |otherwise = (takeWhile (/= c)) str

-- borra de la lista la primera aparicion del argumento que pasamos
delete                ::  (Eq a) => a -> [a] -> [a]
delete _ []        = []
delete x (y:ys)    = if (x == y) then ys else y : delete  x ys
-- diferencia de listas (resta de conjuntos)
listDifference :: (Eq a) => [a] -> [a] -> [a]
listDifference =  foldl (flip (delete))

main :: IO ()
main = do
        xs <- introRel
        if (esRelacion xs) then muestraRelOrd (xs) else putStr ("Los pares introducidos no forman una relacion binaria.\n")
