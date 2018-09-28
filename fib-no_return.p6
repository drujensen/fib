sub fib(Int $n) {
    $n <= 1 ?? 1 !! fib($n - 1) + fib($n - 2);
};

print fib(46)
