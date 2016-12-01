import java.util.*;

public class Fib {
  static int fib(long n) {
    if (n <= 1)
      return 1;
    else
      return fib(n - 1) + fib(n - 2);
  }

  public static void main(String[] args) {
    System.out.print(fib(45));
  }
}
