import Data.Char
import System.IO

let2Int :: Char -> Int
let2Int c = ord c -ord 'a'

int2Let :: Int -> Char
int2Let n = chr (n + ord 'a')

affine :: (Int, Int) -> Int -> Int
affine (a,b) p = (a*p + b) `mod` 26

encryptChar :: (Int, Int) -> Char -> Char
encryptChar (a,b) p = int2Let (affine(a,b) (let2Int p))

encrypt :: (Int, Int) -> String -> String
encrypt (a,b) xs = [encryptChar (a,b) p | p <- xs]

main = do
    putStr "Enter the message you want to encrypt: "
    hFlush stdout
    plainttext <- getLine

    putStr "Enter the coefficient a: "
    hFlush stdout
    mult <- getLine

    putStr "Enter the shift b: "
    hFlush stdout
    shift <- getLine

    let a = read mult :: Int
    let b = read shift :: Int

    let ciphertext = encrypt (a,b) plainttext

    putStr "The ciphertext is: "
    putStrLn ciphertext
