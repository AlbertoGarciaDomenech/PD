import System.IO  
import Control.Monad
--Alberto García Doménech

--1)adivina n para jugar a adivinar un numero.  pedir usuario un numero hasta que acierte con el valor de n.tipo adivina :: Int -> IO ().

pedirnumero:: IO Int
pedirnumero = do line <- getLine
                 return (read line::Int)

adivina :: Int -> IO ()
adivina n = do
    putStr "Introduce que numero crees que estoy pensando: "
    x <- pedirnumero
    if (x < n)then do
        putStrLn "Has introducido un numero menor! "
        adivina n
    else if (x > n) then do
        putStrLn "Has introducido un numero mayor! "
        adivina n
    else do
        putStrLn "Enhorabuena, lo has adivinado!"

-- 2)formatea ::String -> String -> Int -> IO (). formatea in out n formatea a n columnas de ancho cada linea del fichero in y escribe el resultado en out 


formateaLinea linea n = unwords [w ++ p|i <- [0..n], w <- words linea, p <- if (length linea + i) < n then [" "] else [""]]


formatea fileIn fileOut n = do
    let l = []
    handle <- openFile fileIn ReadMode
    contents <- hGetContents handle
    let singlelines = lines contents
    print singlelines
    let singlewords = map words singlelines
    print singlewords
    let nuevasLineas = map formateaLinea singlelines
    -- print nuevasLineas
    writeFile fileOut nuevasLineas:
    hClose handle



-- 3) 


main = do 
 putStr "Introduce el numero de ejercicio que quieres ejecutar: "
 r <- pedirnumero
 if(r == 1) then do
     adivina 3 --numero que tiene que adivinar el usuario
 else if (r==2) then do
     formatea "test.txt" "out.txt" 20
 else do
     putStrLn "Aqui no hay nada"