m = {0: 1, 1: 1}

var fib = function(n) {
  if (n in m) return m[n];
  return m[n] = fib(n - 1) + fib(n - 2);
};

console.log(fib(46));
