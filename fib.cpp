#include <iostream>

long fib(long n) {
    if (n <= 1) {
        return 1;
    }
    return fib(n - 2) + fib(n - 1);
}

int main() {
    std::cout << fib(46) << std::endl;
    return 0;
}
