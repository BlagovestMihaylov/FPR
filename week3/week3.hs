isPerfect :: Int -> Bool
isPerfect n = n == (sumDiv n 1)
    where
        sumDiv  n  start
            | n == start = 0
            | otherwise  = if n `mod` start == 0 then start + sumDiv n (start +1) else sumDiv n (start+1)


--Обратното число 
-- 123 -> 321 

reverseNumber :: Int -> Int
reverseNumber n 
    | n `div` 10  == 0 = n
    | otherwise  = (n `mod` 10 )* (10 ^ (countLenght (n `div` 10))) + reverseNumber (n `div` 10)
        where
            countLenght n = if  (n `div` 10/=0) then 1 + countLenght (n `div` 10) else 1  



main :: IO()
main = do 
    print (isPerfect 6)
    print (reverseNumber 96024)