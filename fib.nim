proc fib(n: uint64): uint64 =
  if n <= 1:
    return 1
  else:
    return fib(n - 1) + fib(n - 2)

proc main =
  echo fib(46)

main()
