import Matriz
import System.Environment --por el getArgs
import Data.List.Split
import Data.List
import System.CPUTime
import Data.Matrix
-------------------------------------
import Control.Parallel.Strategies

evalPair' :: Strategy a -> Strategy a
evalPair' estrategia a = do
    a' <- estrategia a
    return a'

estrats :: Strategy a -> Strategy a
estrats a = evalPair' (rparWith a)
-------------------------------------

aListaDeString :: Matriz -> [[String]]
aListaDeString (Matriz _ _ []) = []
aListaDeString (Matriz filas col mat) = [concat (intersperse " " (map (\x -> show x) (take col mat)))] : (aListaDeString (Matriz filas col (drop col mat)))


productoMatricesEnteras :: Num a => Matrix a -> Matrix a -> Matrix a
productoMatricesEnteras matrizA matrizB = multStd matrizA matrizB

elemento0Tupla :: (a, b) -> a
elemento0Tupla (a,_) = a

elemento1Tupla :: (a, b) -> b
elemento1Tupla (_,b) = b

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
    let matB = Matriz (head matrizB) (head (tail matrizB)) (tail (tail matrizB))
    
    let tuplaMat = runEval $ do

    
        bloquesA <- rpar (aBloques matA (read bx :: Int) (read by :: Int))
        bloquesB <- rpar (aBloques matB (read bx :: Int) (read by :: Int))
        rseq bloquesA
        rseq bloquesB
        return (bloquesA, bloquesB)

    let matrizResultante = aplanar ((multiplicar (elemento0Tupla tuplaMat) (elemento1Tupla tuplaMat)) `using` estrats rseq)

{-     putStr ("\nLa matriz obtenida por multiplicacion por bloques es: (para una mejor vista, ver el archivo de texto creado)  \n")
    print (matrizResultante) -}

    --
    finProducto <- getCPUTime
    let tiempo = (fromIntegral (finProducto - principioProducto))/(10^9)
    putStr ("\nEl tiempo de cómputo en milisegundos es de ")
    print tiempo

    --

    principioEscritura <- getCPUTime
    --
    writeFile fileC ( (show (filasMatriz matrizResultante)) ++ "\n" ++ (show (columnasMatriz matrizResultante)) ++ "\n" ++ (unlines (concat (aListaDeString matrizResultante))))
    --
    finEscritura <- getCPUTime
    let tiempo = (fromIntegral (finEscritura - principioEscritura))/(10^9)
    putStr ("\nEl tiempo de salida en milisegundos es de ")
    print tiempo


    --
    --Producto a comparar:
    {- putStr "\nEl produto de multiplicacion de matrices con las matrices enteras: \n"
    let resultadoAComparar = productoMatricesEnteras (fromList (head matrizA) (head (tail matrizA)) (tail (tail matrizA))) (fromList (head matrizB) (head (tail matrizB)) (tail (tail matrizB)))
    print resultadoAComparar
     -}
    --

    finDeTodo <- getCPUTime
    let tiempo = (fromIntegral (finDeTodo - principioTotal))/(10^9)
    putStr ("\nEl tiempo total en milisegundos es de ")
    print tiempo