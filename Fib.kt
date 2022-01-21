fun fib(n: Long): Long {
        return if (n <= 1) n else fib(n - 1) + fib(n - 2)
}
fun main() {
        print(fib(47))
}
