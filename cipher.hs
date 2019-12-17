
import System.IO
import Affine

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
