# Recursive Fibonacci Benchmark using top languages on Github

Top 10: JavaScript, Java, Python, Ruby, Php, C++, C#, C, Go [reference](http://www.techworm.net/2016/09/top-10-popular-programming-languages-github.html)

Others: Crystal, Rust, Swift, Mono, Elixir, Perl, R, Julia

This code performs a recursive fibonacci to the 46th position with the result of 2,971,215,073.

All tests are run on:
 - iMac (Retina 5K, 27-inch, Late 2015)
 - OS: macOS High Sierra 10.13.5
 - Processor: 3.2 GHz Intel Core i5
 - Memory: 16 GB 1867 MHz DDR3

## Natively compiled, statically typed

| Language  | Time, s | Compile                           | Run          |
|-----------|---------|-----------------------------------|--------------|
| Crystal   |  4.581  | `crystal build --release fib.cr`  | `time ./fib` |
| Rust      |  5.891  | `rustc -O fib.rs`                 | `time ./fib` |
| C++       |  6.265  | `g++ -O3 -o fib fib.cpp`          | `time ./fib` |
| C         |  6.707  | `gcc -O3 -o fib fib.c`            | `time ./fib` |
| Swift     |  9.765  | `swiftc -O -g fib.swift`          | `time ./fib` |
| Go        | 10.607  | `go build fib.go`                 | `time ./fib` |

## VM compiled bytecode, statically typed

| Language  | Time, s | Compile          | Run                 |
|-----------|---------|------------------|---------------------|
| Java      |  7.361  | `javac Fib.java` | `time java Fib`     |
| C# (Mono) | 11.323  | `mcs fib.cs`     | `time mono fib.exe` |
| C#        | 72.486  | `dotnet restore` | `time dotnet run`   |

## VM compiled before execution, mixed/dynamically typed

| Language | Time, s  | Run                  |
|----------|----------|----------------------|
| Dart     |  9.234   | `time dart fib.dart` |
| Julia    | 10.943   | `time julia fib.jl`  |
| Node     | 18.874   | `time node fib.js`   |
| Elixir   | 64.219   | `time elixir fib.exs`|

## Interpreted, dynamically typed

| Language | Time, s  | Run                   |
|----------|----------|-----------------------|
| Ruby     |  195.601 | `time ruby fib.rb`    |
| Php      |  206.346 | `time php fib.php`    |
| Python   |  502.036 | `time python fib.py`  |
| Python3  |  718.681 | `time python3 fib.py` |
| Perl     | 1133.131 | `time perl fib.pl`    |
| R        | 1796.495 | `time r -f fib.r`     |


## Versions

- crystal 0.25.0 (2018-06-15) LLVM: 5.0.1
- go version go1.10.3 darwin/amd64
- g++ Apple LLVM version 9.1.0 (clang-902.0.39.2)
- gcc Apple LLVM version 9.1.0 (clang-902.0.39.2)
- rustc 1.26.2
- swiftc Apple Swift version 4.1.2 (swiftlang-902.0.54 clang-902.0.39.2)
- javac 10.0.1
- mcs Mono C# compiler version 5.12.0.226
- dotnet 2.1.4
- dart VM version: 1.24.3 (Wed Dec 13 23:26:59 2017)
- node v9.4.0
- elixir 1.6.5 (compiled with OTP 20)
- ruby 2.5.1p57 (2018-03-29 revision 63029)
- python 2.7.15
- python3 3.6.5
- php 7.1.16 (cli) (built: Apr  1 2018 13:14:42)
- perl 5, version 26, subversion 2 (v5.26.2)
- r version 3.5.0 (2018-04-23)
- julia version 0.6.3

## Caveats

[Fibonacci Benchmark](https://crystal-lang.org/2016/07/15/fibonacci-benchmark.html)
