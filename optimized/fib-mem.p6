use experimental :cached;

sub fib(Int $n) is cached {
    $n <= 1 ?? 1 !! fib($n - 1) + fib($n - 2);
}

say fib(46)
