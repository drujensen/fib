
func fib(_ n: Int, cache: inout [Int:Int]) -> Int {
    if n <= 1 { return 1 }

    if let f = cache[n] {
        return f
    }

    let a = fib(n - 1, cache: &cache)
    let b = fib(n - 2, cache: &cache)

    let f = a + b
    cache[n] = f
    return f
}

var cache = [Int:Int]()
print(fib(46, cache: &cache ))
