fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib :: Int -> Int
fib i = fibs !! i

main :: IO ()
main = print $ fib 46
