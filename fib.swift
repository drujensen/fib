@main
struct Main {
    static func fib(_ n: Int) -> Int {
        if n <= 1 { return n }
        return fib(n - 1) + fib(n - 2)
    }
    
    static func main() {
        print(fib(47))
    }
}
