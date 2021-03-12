
-- Проверява дали едно число е равно на
-- сбора от делителите си
-- 6 = 1 + 2 + 3

isPerfect :: Int -> Bool
isPerfect n = n == (sumDiv n 1)
    where
        sumDiv  n  start
            | n == start = 0
            | otherwise  = if n `mod` start == 0 then start + sumDiv n (start +1) else sumDiv n (start+1)



-- Обратното число 
-- 123 -> 321 

reverseNumber :: Int -> Int
reverseNumber n 
    | n `div` 10  == 0 = n
    | otherwise  = (n `mod` 10 )* (10 ^ (countLenght (n `div` 10))) + reverseNumber (n `div` 10)
        where
            countLenght n = if  (n `div` 10/=0) then 1 + countLenght (n `div` 10) else 1  

-- Вектори
-- tuple
-- Име, номер, оценка
type Student = (String, Int, Double )

-- fst_3 :: (a, b) -> a
-- fst_3 (x, _) = x       Така изглежда ппц

fst_3 :: Student -> String 
fst_3 (x, _, _) = x    -- _ Означава "не ми пука", връщаме само типа, който искаме

snd_3 :: Student -> Int 
snd_3 (_, x,_) = x     -- връщаме втората координата

trd_3 :: Student -> Double
trd_3 (_, _, x) = x     -- връщаме 3тата коодината на вектора

--възможно е и "типова променлива "
--fst_3 :: (a, b, c) -> c
--fst_3 (_, _,x) = x   връща нещо от типа на променливата С, дори и 
                        -- първообраза да е от различен тип
                        -- изисква само да е наредена тройка; Шаблон

-- събиране на две координати на двойка

type TupleInt = (Int, Int)
addPair :: TupleInt -> Int
addPair (a, b) = a + b
--Typeclass (Haskell) = Concept(c++) 


type Vector = (Double, Double, Double)

sumVectors :: Vector -> Vector ->Vector
sumVectors (a, b, c) (x, y, z) = (a + x, b + y, c + z ) -- сбор на вектори

--scaleVector :: Vector -> Double -> Vector
--scaleVector (a, b, c) (x, y, z) = (a * x, b * y, c * z) -- умножение на вектор със скалар

dotVector :: Vector -> Vector -> Double
dotVector (a, b, c) (x, y, z) = a * x + b * y + c * z --скаларно произведение на вектори

--(a1,a2,a3) *(b1,b2,b3) = (a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1)
crossProduct :: Vector -> Vector -> Vector -- векторно произведение на вектори
crossProduct (a1, a2, a3) (b1, b2, b3) = (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)


main :: IO()
main = do 
    print (isPerfect 6)
    print (reverseNumber 96024) -- обръща номера
    print (trd_3("Gosho", 72085, 6.9420)) -- връща само оценката
    print (addPair (23, 77)) -- връща сбора
    print (sumVectors (1, 2, 3) (6, 5 , 4)) -- връща сбора на векторите 
    print (scaleVector(3, 3, 3) (1, 2, 3))  -- връща умножението на вектор със скалар
    print (dotVector (1, 2, 3) (2, 3, 4)) -- връща скаларното произведение на вектори
    print (crossProduct (1, 2, 3) (4, 4, 4)) -- връща векторното прозиведение на два вектора
    