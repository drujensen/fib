#include <stdio.h>

unsigned int fib(unsigned int n)
{
  if (n <= 1) {
    return 1;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

int main(void) {
  printf("%d", fib(45));
  return 0;
}
