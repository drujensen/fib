# Python >= 3.2 needed.
# https://docs.python.org/3/library/functools.html#functools.lru_cache
from functools import lru_cache

@lru_cache(None)
def fib(n):
  if n <= 1: return 1
  return fib(n - 1) + fib(n - 2)

print(fib(46))
