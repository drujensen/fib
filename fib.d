import std.stdio;

long fib(long n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);
}

void main() {
    writeln(fib(47));
}
