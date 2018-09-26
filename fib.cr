def fib(n : UInt32)
  return 1_u32 if n <= 1
  fib(n - 1) + fib(n - 2)
end

puts fib(46)
