#include <iostream>

constexpr uint32_t fib(uint32_t n) {
    return (n <= 1) ? 1 : fib(n - 1) + fib(n - 2);
}

int main() {
    std::cout << fib(46) << std::endl;
    return 0;
}
