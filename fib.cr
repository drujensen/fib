def fib(n)
  return n if n <= 1
  fib(n - 1) + fib(n - 2)
end

puts fib(47_u64)
