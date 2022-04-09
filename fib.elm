fib x =
  case x of
    0 -> x 
    1 -> x
    _ -> fib (x-1) + fib (x-2)

fib(47)
