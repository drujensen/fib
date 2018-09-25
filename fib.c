#include <stdio.h>
#include <stdint.h>

static uint64_t fib(uint64_t n) {
  if (n <= 1) return 1;
  return fib(n - 1) + fib(n - 2);
}

int main(void) {
  printf("%llu \n", fib(46));
  return 0;
}
