--Първа задача
--А)
isPrime :: Int -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..k-1], mod k x == 0] else False --проверка дали числото е просто

isSafePrime :: Int -> Bool
isSafePrime 2 = True
isSafePrime 1 = False
isSafePrime 0 = False
isSafePrime x = even (x-1) && isPrime x && isPrime (div (x-1) 2) --проверка дали числото 2р+1 е особено, като гледаме дали 
                                                                 --числото е нечетно, дали то е просто и дали числото -1 на 
                                                                 --половина е просто
 

safePrimesCount :: Int -> Int -> Int
safePrimesCount a b =  length [x| x<-[a .. b], isSafePrime x] --проверяваме за всички числа в дадения интервал

--Б)

isPower2:: Int -> Bool 
isPower2 1 = True
isPower2 0 = False
isPower2 x = even x && isPower2 (div x 2) --намираме дали числото е степен на две

isSpecial :: Int -> Bool 
isSpecial 0 = False 
isSpecial 1 = False 
isSpecial 2 = True 
isSpecial x  = isPower2 ( x + 1) && isPrime (floor (logBase 2 (fromIntegral x+1))) --гледаме също дали степента на двойката е просто число 

specialSum:: Int -> Int ->Int
specialSum k m = sum (take k [ x | x<-[m..], isSpecial x ]) --взимаме колкото трябва числа, по-големи от нашето

--втора задача
sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers k = (k `mod` 10) + sumNumbers (k `div` 10) --функция, която намира сбора на цифрите на дадено число

bigger :: Int -> Int
bigger x = if x > 9 then sumNumbers(x) else x --събиране на цифрите на число, по-голямо от 9



digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10] --превръща число в лист от интове

doubleNumber :: Integral x => [x] -> [x]
doubleNumber [] = []
doubleNumber [_] = [x | x<-[2..(lenght(digs)+1)],even x] && digs !! x = digs !! x * 2 --опит за удвояване на 2ри елемент отзад напред


digs' :: Integral x => x -> [x]
digs' 0 = []
digs' x = x `mod` 10 : digs (x `div` 10)  --превръща лист от интове в число


--validate :: Int -> Bool
--validate x = if sumNumbers( digs') `mod` 10 == 0 then True else False

--планът ми като цяло беше да си ги превърна числото в лист 
--   -първо, за да знам дължината му
--   -второ, за да имам отделните елементи
--и след това да мина през листа и да удвоя всяко едно от числата, които трябва
--след това да мина пак през листа с и да отстраня числата, по-големи от 9
--да го превърна в число и да му чъбера цифрите,
--но нещо не можах да направя удвояването

main :: IO()
main = do
   print(safePrimesCount 20 100)
   print(safePrimesCount 1 983)
   print(safePrimesCount 167 1892)
   print(safePrimesCount 1678 20097)
   print(specialSum 3 20)
   print(specialSum 5 31)
   print(specialSum 8 10)
   print(specialSum 10 128)
   print(digs 1234567)
   