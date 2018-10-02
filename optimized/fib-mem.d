module fib_mem;
import std.stdio;
import std.functional;

alias fastFib = memoize!(fib, 46);

long fib(long n) {
  if (n <= 1) return 1;
  return fastFib(n - 1) + fastFib(n - 2);
}

void main() {
    writeln(fastFib(46));
}
