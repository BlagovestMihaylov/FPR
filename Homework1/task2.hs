sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers k = (k `mod` 10) + sumNumbers (k `div` 10)

bigger :: Int -> Int
bigger x = if x > 9 then sumNumbers(x) else x

doubleNumber :: Int -> Int
doubleNumber 0 = 0
doubleNumber x = 
    bigger (2*(x `mod` 10))
    (x `mod` 10) + 20 * doubleNumber(x `div` 10)


main :: IO()
main = do
    --print(sumNumbers 123)
    --print(bigger 18)
    print(doubleNumber 1234567 )
    