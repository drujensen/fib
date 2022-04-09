object Fib {
  def fib(n: Long): Long = if (n <= 1) n else fib(n - 1) + fib(n - 2)

  def main(args: Array[String]): Unit = {
    println(fib(47))
  }
}
