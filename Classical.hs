module Classical
    (
    encryptChar
    ,encrypt
    ) where

import Data.Char

let2Int :: Char -> Int
let2Int c = ord c - ord 'a'

int2Let :: Int -> Char
int2Let n = chr (n + ord 'a')

affine :: (Int, Int) -> Int -> Int
affine (a,b) p = (a*p + b) `mod` 26

encryptChar :: (Int, Int) -> Char -> Char
encryptChar (a,b) p
    |isLower p = int2Let(affine (a,b) (let2Int p))
    |isUpper p = int2Let(affine (a,b) (let2Int(toLower p)))
    |otherwise = p

encrypt :: (Int, Int) -> String -> String
encrypt (a,b) xs = [encryptChar (a,b) p | p <- xs]

shift :: Int -> Char -> Char
shift n p = encryptChar (1,n) p

vigenereEncrypt :: String -> String -> String
vigenereEncrypt kw [] = []
vigenereEncrypt kw xs = cipher ++ vigenereEncrypt kw (drop 4 xs)
    where key = map let2Int kw
          p = take 4 xs
          cipher = zipWith (shift) key p
