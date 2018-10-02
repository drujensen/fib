func fib(n: uint64, cache: var openArray[uint64]): uint64 =
  if n <= 1:
    return 1
  var
    a = cache[n.int - 1]
    b = cache[n.int - 2]
  if a == 0:
    a = fib(n - 1, cache)
    cache[n.int - 1] = a
  if b == 0:
    b = fib(n - 2, cache)
    cache[n.int - 2] = b
  return a + b

var cache: array[46, uint64]
echo fib(46, cache)
