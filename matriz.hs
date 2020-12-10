module Matriz(
Matriz(..)
) where

type Filas = Int
type Columnas = Int

data Matriz = Vacio | Matriz Filas Columnas [Int] | MatrizBloque Filas Columnas [Matriz]
    deriving Show

sacarBloque :: Int -> Int -> Int -> Columnas -> [Int] -> Filas -> Columnas -> [Int]
sacarBloque j i y mx m by bx
    | y < bx = (take bx (drop (mx*by*j + mx*y + bx*i) m)) ++ (sacarBloque j i (y+1) mx m by bx)
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


