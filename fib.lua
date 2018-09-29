local function fib(n)
    if n <= 1 then return 1 end
    return fib(n - 1) + fib(n - 2)
end

print(fib(46))
