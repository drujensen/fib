#include <iostream>

using namespace std;

constexpr long fib(long x) {
  if (x <= 1) { return 1; }
  return fib(x - 1) + fib(x - 2);
}

int main()
{
  std::out << fib(46) << std::endl;
}
