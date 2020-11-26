--Lee dos 3 y dos numeros, lee el primer y segundo archivo, sobrescribe el tercero
import System.Environment --por el getArgs

main :: IO ()
main = do
    [fileA,fileB,fileC,bx,by] <- getArgs
    matrizA <- readFile fileA 
    matrizB <- readFile fileB
    --se hace lo que se tiene que hacer y retorna resultado

    --Para que no de error por mientras
    let resultado = "sdfsdf"
    --
    putStrLn matrizA
    putStrLn matrizB
    --

    --writeFile resultado fileC