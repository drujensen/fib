def fib(n : UInt64)
  if n <= 1
    1_u64
  else
    fib(n - 1) + fib(n - 2)
  end
end

puts fib(46)
