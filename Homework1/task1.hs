--А)
isPrime :: Int -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..k-1], mod k x == 0] else False

isSafePrime :: Int -> Bool
isSafePrime 2 = True
isSafePrime 1 = False
isSafePrime 0 = False
isSafePrime x = even (x-1) && isPrime x && isPrime (div (x-1) 2)
 

safePrimesCount :: Int -> Int -> Int
safePrimesCount a b =  length [x| x<-[a .. b], isSafePrime x]

--Б)

isPower2:: Int -> Bool 
isPower2 1 = True
isPower2 0 = False
isPower2 x = even x && isPower2 (div x 2)

isSpecial :: Int -> Bool 
isSpecial 0 = False 
isSpecial 1 = False 
isSpecial 2 = True 
isSpecial x  = isPower2 ( x + 1) && isPrime (floor (logBase 2 (fromIntegral x+1)))

specialSum:: Int -> Int ->Int
specialSum k m = sum (take k [ x | x<-[m..], isSpecial x ])

main :: IO()
main = do
    print(safePrimesCount 20 100)
    print(safePrimesCount 1 983)
    print(safePrimesCount 167 1892)
    print(safePrimesCount 1678 20097)
    print(specialSum 3 20)
    --print(specialSum 5 31)
    --print(specialSum 8 10)
    print(specialSum 10 128)

