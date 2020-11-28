--Lee dos 3 y dos numeros, lee el primer y segundo archivo, sobrescribe el tercero
import System.Environment --por el getArgs
import Data.List.Split

main :: IO ()
main = do
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
    
    print matrizA
    print matrizB
    --
