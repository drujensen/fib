#include <iostream>

long fib(long n) {
  if (n <= 1) return 1;
  return fib(n - 1) + fib(n - 2);
}

int main() {
  std::cout << fib(46) << std::endl;
}
