module Matriz(
Matriz(..)
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

filasBloque :: Matriz -> Int
filasBloque (Matriz by _ _)  = by

columnasBloque :: Matriz -> Int
columnasBloque (Matriz _ bx _) = bx

matriz :: Matriz -> [Int]
matriz (Matriz _ _ m) = m

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
                            let by = (filasBloque . head) m
                                bx = (columnasBloque . head) m
                                my = y*by
                                mx = x*bx
                            in
                            (Matriz my mx (aplanar' 0 0 0 by bx y x m))
