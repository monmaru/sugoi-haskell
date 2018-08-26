import           Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    c <- logNumber 8
    tell ["Gonna multiply three five eight"]
    return (a * b * c)

{-
*Main> runWriter multWithLog
(120,["Got number: 3","Got number: 5","Got number: 8","Gonna multiply three five eight"])
-}


gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

{-
*Main> runWriter (gcd' 8 3)
(1,["8 mod 3 = 2","3 mod 2 = 1","2 mod 1 = 0","Finished with 1"])
-}
