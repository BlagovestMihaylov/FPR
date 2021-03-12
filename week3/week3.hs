isPerfect :: Int -> Bool
isPerfect n = n == (sumDiv n 1)
    where
        sumDiv  n  start
            | n == start = 0
            | otherwise  = if n `mod` start == 0 then start + sumDiv n (start +1) else sumDiv n (start+1)

main :: IO()
main = do 
    print (isPerfect 6)