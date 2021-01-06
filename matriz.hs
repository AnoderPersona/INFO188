module Matriz(
Matriz(..),
aplanar,
sacarBloque,
aBloques,
multiplicar,
columnasMatriz,
filasMatriz
) where

type Filas    = Int
type Columnas = Int

data Matriz = Vacio | Matriz Filas Columnas [Int] | MatrizBloque Filas Columnas [Matriz]
    deriving Show


sacarBloque :: Filas -> Columnas -> Int -> Columnas -> [Int] -> Filas -> Columnas -> [Int]
sacarBloque j i y mx m by bx
    | y < bx    = (take bx (drop (mx*by*j + mx*y + bx*i) m)) ++ (sacarBloque j i (y+1) mx m by bx)
    | otherwise = []

aBloques' :: Int -> Int -> Filas -> Columnas -> [Int] -> Filas -> Columnas -> [Matriz]
aBloques' j i my mx m by bx
    | i <    mbx    = (Matriz by bx (sacarBloque j i 0 mx m by bx)) : (aBloques'   j  (i+1) my mx m by bx)
    | j < (mby - 1) = aBloques' (j+1)  0   my mx m by bx
    | otherwise     = []
    where mbx = mx `div` bx
          mby = my `div` by

aBloques :: Matriz -> Filas -> Columnas -> Matriz
aBloques (Matriz my mx m) by bx = MatrizBloque (my `div` by) (mx `div` bx) (aBloques' 0 0 my mx m by bx)




filasMatriz :: Matriz -> Int
filasMatriz    (Matriz by _ _)    = by
filasMatriz (MatrizBloque by _ _) = by

columnasMatriz :: Matriz -> Int
columnasMatriz    (Matriz _ bx _)    = bx
columnasMatriz (MatrizBloque _ bx _) = bx

matriz :: Matriz -> [Int]
matriz    (Matriz _ _ m)    = m


sacarFila :: Filas -> Columnas -> Int -> Filas -> Columnas -> Columnas -> [Matriz] -> [Int]
sacarFila j i y by bx mx m = (take bx (drop (bx*y) (matriz (head (drop (mx*j+i) m)))))

aplanar' :: Filas -> Columnas -> Int -> Filas -> Columnas -> Filas -> Columnas -> [Matriz] -> [Int]
aplanar' j i y by bx my mx m
    | i <    mx    = (sacarFila j i y by bx mx m) ++ (aplanar' j (i+1) y by bx my mx m)
    | y < (by - 1) = aplanar'   j   0 (y+1) by bx my mx m
    | j < (my - 1) = aplanar' (j+1) 0   0   by bx my mx m
    | otherwise     = []

aplanar :: Matriz -> Matriz
aplanar (MatrizBloque y x m) =
                            let by = (filasMatriz . head) m
                                bx = (columnasMatriz . head) m
                                my = y*by
                                mx = x*bx
                            in
                            (Matriz my mx (aplanar' 0 0 0 by bx y x m))



matrizNula' :: Int -> [Int]
matrizNula' 0 = []
matrizNula' x = 0 : (matrizNula' (x-1))

matrizNula :: Int -> Int -> Matriz
matrizNula y x = (Matriz y x (matrizNula' (y*x)))

sumar :: Matriz -> Matriz -> Matriz
sumar (Matriz my1 mx1 m1) (Matriz my2 mx2 m2) = (Matriz my1 mx1 (zipWith (+) m1 m2))
sumar (MatrizBloque my1 mx1 m1) (MatrizBloque my2 mx2 m2) = (MatrizBloque my1 mx1 (zipWith (sumar) m1 m2))

transponer' :: Filas -> Columnas -> Filas -> Columnas -> [a] -> [a]
transponer' j i y x m
    | j <   y   = m!!(j+j*(x-1)+i) : (transponer' (j+1) i y x m)
    | i < (x-1) = transponer' 0 (i+1) y x m
    | otherwise = []

transponer :: Matriz -> Matriz
transponer (Matriz y x m)       = Matriz x y (transponer' 0 0 y x m)
transponer (MatrizBloque y x m) = MatrizBloque x y (transponer' 0 0 y x m)

multiplicar'' :: Int -> Int -> Int -> Matriz -> Matriz -> Int
multiplicar'' x j i (Matriz my1 mx1 m1) (Matriz mx2 my2 m2)
    | x <   mx1   = (m1!!(x+mx1*j) * m2!!(x+my2*i)) + (multiplicar'' (x+1) j i (Matriz my1 mx1 m1) (Matriz mx2 my2 m2))
    | otherwise   = 0

multiplicarB'' :: Int -> Int -> Int -> Matriz -> Matriz -> Matriz
multiplicarB'' x j i (MatrizBloque my1 mx1 m1) (MatrizBloque mx2 my2 m2)
    | x <   mx1   = (sumar (multiplicar (m1!!(x+mx1*j)) (m2!!(x+my2*i))) (multiplicarB'' (x+1) j i (MatrizBloque my1 mx1 m1) (MatrizBloque mx2 my2 m2)))
    | otherwise   = (matrizNula my1 mx2)

multiplicar' :: Int -> Int -> Matriz -> Matriz -> [Int]
multiplicar' y x (Matriz my1 mx1 m1) (Matriz mx2 my2 m2)
    | x <    mx2    = (multiplicar'' 0 y x (Matriz my1 mx1 m1) (Matriz mx2 my2 m2)) : (multiplicar' y (x+1) (Matriz my1 mx1 m1) (Matriz mx2 my2 m2))
    | y < (my1 - 1) = multiplicar' (y+1) 0 (Matriz my1 mx1 m1) (Matriz mx2 my2 m2)
    | otherwise     = [] 

multiplicarB' :: Int -> Int -> Matriz -> Matriz -> [Matriz]
multiplicarB' y x (MatrizBloque my1 mx1 m1) (MatrizBloque mx2 my2 m2)
    | x <    mx2    = (multiplicarB'' 0 y x (MatrizBloque my1 mx1 m1) (MatrizBloque mx2 my2 m2)) : (multiplicarB' y (x+1) (MatrizBloque my1 mx1 m1) (MatrizBloque mx2 my2 m2))
    | y < (my1 - 1) = multiplicarB' (y+1) 0 (MatrizBloque my1 mx1 m1) (MatrizBloque mx2 my2 m2)
    | otherwise     = []

multiplicar :: Matriz -> Matriz -> Matriz
multiplicar (Matriz my1 mx1 m1) (Matriz my2 mx2 m2) = Matriz my1 mx2 (multiplicar' 0 0 (Matriz my1 mx1 m1) (transponer (Matriz my2 mx2 m2)))
multiplicar (MatrizBloque my1 mx1 m1) (MatrizBloque my2 mx2 m2) = MatrizBloque my1 mx2 (multiplicarB' 0 0 (MatrizBloque my1 mx1 m1) (transponer (MatrizBloque my2 mx2 m2)))
