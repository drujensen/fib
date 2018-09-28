fib :: Int -> Int
fib i | i <= 1= 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = print $ fib 46
