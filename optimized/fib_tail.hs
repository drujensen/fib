{-# LANGUAGE BangPatterns #-}

fib :: Int -> Int
fib i = go 0 1 i
    where
        go !a !b !0 = a
        go !a !b !1 = b
        go !a !b !i = go b (a+b) (i-1)

main :: IO ()
main = print $ fib $ 46
