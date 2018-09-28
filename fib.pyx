# Usage:
#   cython3 --embed -o fib.pyx.c fib.pyx
#   g++ -O3 -o fib fib.pyx.c -I/usr/include/python3.5m/ -lpython3.5m
#   time ./fib

cdef long fib(long n):
  if n <= 1: return 1
  return fib(n - 1) + fib(n - 2)

print(fib(46))
