const fib = function (n, cache) {
  if (n <= 1) {
    return 1
  }
  let a = cache[n - 1]
  let b = cache[n - 2]
  if (!a) {
    a = fib(n - 1, cache)
    cache[n - 1] = a
  }
  if (!b) {
    b = fib(n - 2, cache)
    cache[n - 2] = b
  }
  return a + b
}

const cache = new Array(46)
console.log(fib(46, cache))
