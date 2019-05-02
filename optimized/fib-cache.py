from functools import lru_cache

@lru_cache(3)
def fib(n):
  if n <= 1: return 1
  return fib(n - 1) + fib(n - 2)

print(fib(46))
