#include <iostream>

using namespace std;

long fib(long x) {
    if (x <= 1) {
        return 1;
    } else {
        return fib(x - 1) + fib(x - 2);
    }
}

int main()
{
    cout << fib(46);
}
