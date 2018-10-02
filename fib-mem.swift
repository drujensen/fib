func fib(n: Int, cache: inout [Int]) -> Int {
    if n <= 1 { return 1 }
    if cache[n-1] == 0 {
        cache[n-1] = fib(n: n-1, cache: &cache)
    }
    if cache[n-2] == 0 {
        cache[n-2] = fib(n: n-2, cache: &cache)
    }
    return cache[n-1] + cache[n-2]
}

var cache = Array.init(repeating: 0, count: 46)
print(fib(n: 46, cache: &cache))
