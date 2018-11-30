class Global
  class_property cache = {0_u64 => 0_u64, 1_u64 => 1_u64}
end

def fib(n : UInt64)
  return 1_u64 if n <= 1
  return Global.cache[n] if Global.cache[n]?
  Global.cache[n] = fib(n - 1) + fib(n - 2)
end

puts fib(46)

