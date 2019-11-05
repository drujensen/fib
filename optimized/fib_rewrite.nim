template fibFast* {fib(n - 1) + fib(n - 2)}(n: int): int = 2 * fib(n - 2) + fib(n - 3)

proc fib(n: int): int =
  if n > 2 : return fib(n - 1) + fib(n - 2)
  if n > 0 : return 1
  

echo fib(46)
