null' :: [a] ->Bool --проверява дали е празен списъка 
null' [] = True
null' _ = False

head' :: [a] -> a
head' (x:xs) = x   --x е главата на списъка, xs е остатъка от списъка 
                   -- в abc, а е х, bc e xs

tail' :: [a] -> [a]  
tail' (x:xs) = xs

sum' :: [Int]-> Int --Int, защото не всеки тип може да се събира
sum' [] = 0
sum' (x:xs) = x + sum' xs

lenght' :: [a] -> Int
lenght' list = helper 0 list      --итеративно решение, важно helper да започва от 0
    where
        helper len [] = len
        helper len (x:xs) = helper (len + 1) xs

lenght'' :: [a] -> Int
lenght'' [] = 0                  --рекурсивно решение
lenght'' (x:xs) = 1 + lenght'' xs

elem' :: (Eq a)=> a -> [a] -> Bool --Eq задължава типа на а да те такъв, който поддържа релация на еквивалентност 
elem' x [] = False                 -- или по-точно, може да се използва == с този тип
elem' x(y:ys) = if x == y then True else (elem' x ys)  

main :: IO()
main = do
    print(null' ["a", "b", "c"]) 
    print("head":["a", "b", "c"]) --така добавяме елемент в началото на списъка "евтино добавяне"
    print(head' ["a", "b", "c"]) --връща а
    print(tail' ["a", "b", "c"]) --връща bc
    print(sum' [1, 2, 3]) --връща 6
    print(lenght' ["a", "b", "c"])  --връща 3
    print(lenght'' ["a", "b", "c"]) -- връща 3
    print(elem' 4 [1, 2, 3]) --изкарва False, защото 4 не е от списъка
    print(elem' ("a", "a") [("a", "a"), ("a", "a")]) --работи и с вектори, но трябва да са
                                                     --с еднакъв брой елемети
                                                     --"а", "а" е наредена двойка
                                                     --и може да се търси само във вектор
                                                     -- с наредени двойки