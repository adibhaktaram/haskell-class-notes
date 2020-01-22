import Control.Monad.Random

getNum:: (RandomGen g) => Integer -> Rand g Integer
getNum n = getRandomR(2,n)

main = do
    randNum <- evalRandIO(getNum 13)
    let numString = show randNum :: String

    putStrLn(numString)
