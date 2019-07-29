fn fib(n i64) i64 {
    if n <= 1 {
        return 1
    }
    return fib(n - 1) + fib(n - 2)
}

fn main() {
    println(fib(46))
}
