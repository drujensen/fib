#include <stdio.h>

long fib(long n)
{
  if (n <= 1) {
    return 1;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

int main(void) {
  printf("%li", fib(46));
  return 0;
}
