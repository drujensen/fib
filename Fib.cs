using System;

public class Fib
{
  public static uint fib(uint n)
  {
    if (n <= 1)
    {
        return 1;
    }
    else
    {
        return fib(n - 1) + fib(n - 2);
    }
  }

  static void Main(string[] args)
  {
    Console.WriteLine(fib(45));
  }
}
