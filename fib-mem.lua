local nums = {}
local fib

fib = function(n)
  if n <= 1 then
    return 1
  else
    if nums[n] then
      return nums[n]
    else
      nums[n] = fib(n - 1) + fib(n - 2)
      return nums[n]
    end
  end
end

print(fib(46))
