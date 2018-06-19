sub fib {
    my $n = shift;
    if ( $n <= 1 ) {
        return 1;
    }
    else {
        return fib($n - 1) + fib($n - 2);
    }
}

print fib(46)
