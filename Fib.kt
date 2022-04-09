fun fib(n: Long): Long {
    if (n <= 1) return n
    return fib(n - 1) + fib(n - 2)
}

fun main() {
    print(fib(47))
}
