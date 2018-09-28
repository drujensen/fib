@mem = [0, 1]
def fib(n)
    if n == 0
        return 1
    end
  return @mem[n] if @mem[n]
  @mem[n] = fib(n - 1) + fib(n - 2)
end

puts fib(46)