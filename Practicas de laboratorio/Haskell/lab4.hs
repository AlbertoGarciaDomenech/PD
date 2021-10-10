--Alberto Garcia Domenech

--1) Define Direccion(arriba,abajo,izquierda,derecha). Instancia de Eq,Ord,Show. Funcion destino

data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Eq,Ord,Show)
type Punto = (Float,Float)

destino :: Punto -> [Direccion] -> Punto
destino (a,b) [] = (a,b)
destino (a,b) (Arriba:xs) = destino (a,b+1) xs
destino (a,b) (Abajo:xs) = destino (a,b-1) xs
destino (a,b) (Derecha:xs) = destino (a+1,b) xs
destino (a,b) (Izquierda:xs) = destino (a-1,b) xs

--2)Tipo Nat sucesion Peano. Instancia Eq, Ord

data Nat = Cero | S Nat deriving (Eq,Ord)

-- infix 4 misuma
misuma :: Nat -> Nat -> Nat
misuma Cero a = a
misuma (S a) b =  S((misuma) a b)
-- infix 5 mimul
mimul :: Nat -> Nat -> Nat
mimul Cero a =  Cero
mimul (S a) b = (misuma) ((mimul) a b) b

nattoInt :: Nat -> Int
nattoInt Cero = 0
nattoInt (S n) = succ (nattoInt n)

instance Show Nat where
    show Cero = "CeroNAT"
    show (S n) = show (succ (nattoInt n))

-- 3)Define tipo para representar complejos. Instancia Eq,Num,Show

data Complejo = Constr Float Float 

instance Num Complejo where
    (+) (Constr a b) (Constr c d) = (Constr (a+b) (c+d))
    (-) (Constr a b) (Constr c d) = (Constr (a-b) (c-d))
    (*) (Constr a b) (Constr c d) = (Constr (a*c - b*d) (a*d + b*c))

instance Eq Complejo where
   (==) (Constr a b) (Constr c d) = ((a == c) && (b == d))
   (/=) (Constr a b) (Constr c d) = ((a /= c) || (b /= d))

instance Show Complejo where
   show (Constr a b) = (show a) ++ " + " ++(show b) ++ "i"

-- 4)Clase de tipos Medible, funcion medida::a->Int. Declara algunos tipos como instancia de Medible como Bool (False = 0, True = 1), [a], (a.b)
class Medible a where
    tam :: a -> Int
instance Medible Int where
    tam a = a
instance Medible Bool where
    tam False = 0
    tam True = 1
instance (Medible a) => Medible [a] where
    tam [] = 0
    tam (x:xs) = tam x + tam xs
instance (Medible a,Medible b) => Medible (a,b) where
    tam (a,b) = tam a + tam b