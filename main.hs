--Lee dos 3 y dos numeros, lee el primer y segundo archivo, sobrescribe el tercero
import System.Environment --por el getArgs
import Data.List.Split
import System.CPUTime
import matriz

aListaDeString :: Matriz -> [[String]]
aListaDeString (Matriz _ _ []) = []
aListaDeString (Matriz filas col mat) = [concat (intersperse " " (map (\x -> show x) (take col mat)))] : (aListaDeString (Matriz filas col (drop col mat)))

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

    print matrizA
    print matrizB
    
    --

    principioProducto <- getCPUTime
    --
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

    finDeTodo <- getCPUTime
    let tiempo = (fromIntegral (finDeTodo - principioTotal))/(10^9)
    putStr ("El tiempo total en milisegundos es de ")
    print tiempo