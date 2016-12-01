# Recursive Fibonacci using top languages on github
js, java, python, ruby, php, c++, c#, c, go, swift, rust

[reference](http://www.techworm.net/2016/09/top-10-popular-programming-languages-github.html)

Including crystal-lang for comparison

## Natively compiled, statically typed

| Language  | Time, s | Compile                          | Run          | Overflow |
|-----------|---------|----------------------------------|--------------|----------|
| C         | 6.999   | `gcc -o fib fib.c`               | `time ./fib` |          |
| C++       | 7.166   | `g++ -o fib fib.cpp`             | `time ./fib` |          |
| Go        | 6.703   | `go build fib.go`                | `time ./fib` |    X     |
| Rust      | 12.038  | `rustc fib.rs`                   | `time ./fib` |    X     |
| Swift     | 13.012  | `swiftc -g fib.swift`            | `time ./fib` |    X     |
|**Crystal**|**3.857**| `crystal build fib.cr --release` | `time ./fib` |          |

## VM bytecode, statically typed

| Language      | Time, s | Compile          | Run                 | Overflow |
|---------------|---------|------------------|---------------------|----------|
| C#            | 43      | `dotnet restore` | `time dotnet run`   |    X     |
| C# using Mono | 7.166   | `mcs fib.cs`     | `time mono fib.exe` |    X     |
| Java          | 4.672   | `javac Fib.java` | `time java Fib`     |          |

## Interpreted, dynamically typed

| Language | Time                 | Run                 | Overflow |
|----------|----------------------|---------------------|----------|
| Node     | 12.76 Seconds        | `time node fib.js`  |    X     |
| Ruby     | 2 Minutes 4 Seconds  | `time ruby fib.rb`  |    X     |
| Python   | 5 Minutes 44 Seconds | `time python fib.py`|    X     |
| PHP      | 6 Minutes 2 Seconds  | `time php fib.php`  |    X     |

## Caveats

[Fibonacci Benchmark](https://crystal-lang.org/2016/07/15/fibonacci-benchmark.html)

Languages that do not handle overflow protection: C, C++, Crystal, Java

Note: Java doesn't support Unsigned Int.
