# Recursive Fibonacci Benchmark using top languages on Github

Top 10: JavaScript, Java, Python, Ruby, Php, C++, C#, C, Go [reference](http://www.techworm.net/2016/09/top-10-popular-programming-languages-github.html)

Others: Crystal, Rust, Swift, Mono, Elixir

This code performs a recursive fibonacci to the 46th position with the result of 2,971,215,073.

All tests are run on:
 - iMac (Retina 5K, 27-inch, Late 2015)
 - OS: macOS Sierra 10.12.1
 - Processor: 3.2 GHz Intel Core i5
 - Memory: 16 GB 1867 MHz DDR3

## Natively compiled, statically typed

| Language  | Time, s | Compile                                    | Run          |
|-----------|---------|--------------------------------------------|--------------|
| C         | 5.459   | `gcc -O3 -o fib fib.c`                     | `time ./fib` |
| C++       | 5.487   | `g++ -O3 -o fib fib.cpp`                   | `time ./fib` |
| Crystal   | 5.500   | `crystal build fib.cr --release`           | `time ./fib` |
| Rust      | 6.077   | `rustc -O fib.rs`                          | `time ./fib` |
| Swift     | 8.576   | `swiftc -O -g Fib.swift`                   | `time ./fib` |
| Go        | 10.621  | `go build fib.go`                          | `time ./fib` |

## VM compiled bytecode, statically typed

| Language  | Time, s | Compile          | Run                 |
|-----------|---------|------------------|---------------------|
| Java      | 6.553   | `javac Fib.java` | `time java Fib`     |
| C# (Mono) | 11.161  | `mcs fib.cs`     | `time mono fib.exe` |
| C#        | 72.410  | `dotnet restore` | `time dotnet run`   |

## VM compiled before execution, mixed typed

| Language | Time                 | Run                  |
|----------|----------------------|----------------------|
| Dart     | 8.936 Seconds        | `time dart fib.dart` |
| Node     | 19.960 Seconds       | `time node fib.js`   |

## Interpreted, dynamically typed

| Language | Time                 | Run                  |
|----------|----------------------|----------------------|
| Elixir   | 1 Minutes 6 Seconds  | `time elixir Fib.exs`|
| Ruby     | 3 Minutes 18 Seconds | `time ruby fib.rb`   |
| Python   | 9 Minutes 13 Seconds | `time python fib.py` |
| Php      | 9 Minutes 34 Seconds | `time php fib.php`   |

## Versions

- crystal 0.20.3 (2016-12-23)
- go version go1.7.3 darwin/amd64
- g++ Apple LLVM version 8.0.0 (clang-800.0.42.1)
- gcc Apple LLVM version 8.0.0 (clang-800.0.42.1)
- rustc 1.13.0
- swiftc Apple Swift version 3.0.2 (swiftlang-800.0.63 clang-800.0.42.1)
- javac 1.8.0_73
- mcs Mono C# compiler version 4.2.2.0
- dotnet 1.0.0-preview2-1-003177
- dart VM version: 1.21.0 (Wed Dec  7 06:44:15 2016)
- node v6.1.0
- elixir 1.3.4
- ruby 2.3.1p112
- python 2.7.10
- php 5.6.25 (cli) (built: Sep  6 2016 16:37:16)

## Caveats

[Fibonacci Benchmark](https://crystal-lang.org/2016/07/15/fibonacci-benchmark.html)


