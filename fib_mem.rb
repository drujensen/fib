@cache = [0, 1]
def fib(n)
  return 1 if n <= 1
  return @cache[n] if @cache[n]
  @cache[n] = fib(n - 1) + fib(n - 2)
end

puts fib(46)