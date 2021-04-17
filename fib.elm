fib x =
  case x of
    0 -> 1
    1 -> 1
    _ -> fib (x-1) + fib (x-2)

fib(46)
