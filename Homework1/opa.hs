averageFunction :: [a -> b] -> Int -> Double
averageFunction fs n = average(x <- fs)

main :: IO()
main = do
    print $ averageFunction [(+1), (**0.5), (2**)] 
