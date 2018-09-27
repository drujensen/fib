using System;
using System.Runtime.CompilerServices;

public class Fib {
  [MethodImpl(MethodImplOptions.AggressiveInlining)]
  static uint fib(uint n) {
    if (n <= 1) return 1;
    return fib(n - 1) + fib(n - 2);
  }

  public static void Main(string[] args) {
    Console.WriteLine(fib(46));
  }
}
