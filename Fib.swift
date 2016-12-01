func fib(n: Int) -> Int {
    if n <= 1 {
        return 1
    } else {
        return (fib(n: n - 1) + fib(n: n - 2))
    }
}

print(fib(n: 45))
