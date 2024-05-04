fn fib(n: UInt64) -> UInt64:
  if n <= 1: return n
  return fib(n - 1) + fib(n - 2)

fn main():
  print(fib(47))
