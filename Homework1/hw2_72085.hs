
import Data.List.Split

countRats :: String -> Int
countRats [] = 0
countRats xs = count '(' (a!!0) + count ')' (a!!1) --броя отварящите скоби преди Р и
    where                                          --и затварящите след Р ... броя скобите, а не "1)" или "(1", защото в условието пише, че ще са правилни входни данни
        a = splitOn "P" xs  --разделям стринга на два малки - единия с елементи преди Р и други с елементите след Р
        count :: Char -> String -> Int
        count x [] = 0
        count x (c:cs) | x == c = 1 + count x cs  --проста функция за броене колко пъти елемент го има в стринг
                       | otherwise = count x cs
    

remove :: Int -> [a] -> [a]
remove n xs = let (as, bs) = splitAt n xs in as ++ tail bs  --премахване на елемнт като разделям списъка на два
                                                            -- и ги събирам после

generateOrder::(Eq a) => [a] -> Int -> Int -> Int -> [a] --функция, която показва реда на децата
generateOrder xs x step len
    |x < 0     = error "k was not natural"
    |xs == []  = []
    |otherwise      = (xs!!(index)):(generateOrder (remove (index) xs) stroke step (len ))
        where
            stroke = if((x + step -1)  <= (length(xs)) ) then (x + step - 1) else mod (x + step  ) (length(xs) ) --stroke e стъпката при въртене на списъка, степ е стъпката, с която се върти списъка
            index = mod (x - step + step -1) (length(xs))  -- позицията на текущия елемент

josephus::(Eq a) => [a] -> (Int -> [a])
josephus xs = (\x -> generateOrder xs x x (length(xs)))

main :: IO()
main = do
    print(countRats "1()1)1)11(1()1)1P)11()1)1)11(1(1(1(")  -- 7
    print(josephus [1..7] 3)   -- [3,6,2,7,5,1,4]
    print(josephus [1..10] 2)  -- [2,4,6,8,10,3,7,1,9,5]
    print(josephus [1..10] 1)  -- [1,2,3,4,5,6,7,8,9,10]  
    print(josephus "fpFMIsu" 4) --"MfsIuFp"
    --не можах да оправя това да работи за прозиволен Инт, работи до н < дължината/2 

    