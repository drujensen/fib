@main
struct Main {
    static func fib(_ n: Int, _ cache: inout [Int]) -> Int {
        if n <= 1 { return n }
        
        if cache[n - 1] == 0 {
            cache[n - 1] = fib(n - 1, &cache)
        }
        
        if cache[n - 2] == 0 {
            cache[n - 2] = fib(n - 2, &cache)
        }
        
        return cache[n - 1] + cache[n - 2]
    }
    
    static func main() {
        var cache = Array.init(repeating: 0, count: 46)
        print(fib(46, &cache))
    }
}
