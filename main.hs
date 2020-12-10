import Matriz
import System.Environment --por el getArgs
import Data.List.Split
import Data.List
import System.CPUTime
import Data.Matrix

aListaDeString :: Matriz -> [[String]]
aListaDeString (Matriz _ _ []) = []
aListaDeString (Matriz filas col mat) = [concat (intersperse " " (map (\x -> show x) (take col mat)))] : (aListaDeString (Matriz filas col (drop col mat)))

productoMatricesEnteras :: Num a => Matrix a -> Matrix a -> Matrix a
productoMatricesEnteras matrizA matrizB = multStd matrizA matrizB

main :: IO ()
main = do

    principioTotal <- getCPUTime
    --

    principioLectura <- getCPUTime
    --
    [fileA,fileB,fileC,bx,by] <- getArgs
    archivoA <- readFile fileA 
    archivoB <- readFile fileB
    --
    let linesA = lines archivoA
    let matrizA =  (read (head linesA)::Int) : (read ((head . tail) linesA)::Int) : map (\x -> read x::Int) ((init . splitOn " " . foldl1 (++) . map (\x -> x ++ " ") . tail . tail) linesA)
    --
    let linesB = lines archivoB
    let matrizB =  (read (head linesB)::Int) : (read ((head . tail) linesB)::Int) : map (\x -> read x::Int) ((init . splitOn " " . foldl1 (++) . map (\x -> x ++ " ") . tail . tail) linesB)
    --
    finLectura <- getCPUTime
    let tiempo = (fromIntegral (finLectura - principioLectura))/(10^9)
    putStr ("El tiempo de entrada en milisegundos es de ")
    print tiempo
    
    --

    
    --

    principioProducto <- getCPUTime
    --
    let matA = Matriz (head matrizA) (head (tail matrizA)) (tail (tail matrizA))
    let bloquesA = aBloques matA

    let matB = Matriz (head matrizB) (head (tail matrizB)) (tail (tail matrizB))
    let bloquesB = aBloques matB


    print "La matriz obtenida por multiplicación por bloques es:"

    --multiplicacion de matrices
    --
    finProducto <- getCPUTime
    let tiempo = (fromIntegral (finProducto - principioProducto))/(10^9)
    putStr ("El tiempo de cómputo en milisegundos es de ")
    print tiempo

    --

    principioEscritura <- getCPUTime
    --
    writeFile fileC (unlines (concat (aListaDeString matrizResultante)))
    --
    finEscritura <- getCPUTime
    let tiempo = (fromIntegral (finEscritura - principioEscritura))/(10^9)
    putStr ("El tiempo de salida en milisegundos es de ")
    print tiempo


    --
    --Producto a comparar:
    print ("El produto de multiplicacion de matrices con las matrices enteras: ")
    print (productoMatricesEnteras (fromList (head matrizA) (head (tail matrizA)) (tail (tail matrizA))) (fromList (head matrizB) (head (tail matrizB)) (tail (tail matrizB))))
    
    --

    finDeTodo <- getCPUTime
    let tiempo = (fromIntegral (finDeTodo - principioTotal))/(10^9)
    putStr ("El tiempo total en milisegundos es de ")
    print tiempo