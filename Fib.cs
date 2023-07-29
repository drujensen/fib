using System;

public class Fib {
  static uint fib(ulong n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
  }

  public static void Main(string[] args) {
    Console.WriteLine(fib(47));
  }
}
