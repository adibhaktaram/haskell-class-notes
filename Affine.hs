module Affine
    (
    encryptChar
    ,encrypt
    ) where

import Data.Char

let2Int :: Char -> Int
let2Int c = ord c -ord 'a'

int2Let :: Int -> Char
int2Let n = chr (n + ord 'a')

affine :: (Int, Int) -> Int -> Int
affine (a,b) p = (a*p + b) `mod` 26

encryptChar :: (Int, Int) -> Char -> Char
encryptChar (a,b) p
    |isLower p = int2Let(affine (a,b) (let2Int p))
    |isUpper p = int2Let(affine (a,b)(let2Int (toLower p)))
    |otherwise = p

encrypt :: (Int, Int) -> String -> String
encrypt (a,b) xs = [encryptChar (a,b) p | p <- xs]
