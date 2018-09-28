#include <stdio.h>

unsigned long fib(unsigned long n) {
  if (n <= 1) return 1;
  return fib(n - 1) + fib(n - 2);
}

int main(void) {
  printf("%lu", fib(46));
  return 0;
}
