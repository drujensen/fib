fun fib(n: Long): Long {
        return if (n <= 1) 1 else fib(n - 1) + fib(n - 2)
}
fun main() {
        print(fib(46))
}
