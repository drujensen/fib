# Recursive Fibonacci using top languages on github
js, java, python, ruby, php, c++, c#, c, go, swift, rust

[reference](http://www.techworm.net/2016/09/top-10-popular-programming-languages-github.html)

Including crystal-lang for comparison

## Natively compiled, statically typed

| Language  | Time, s | Compile                          | Run          |
|-----------|---------|----------------------------------|--------------|
| C         | 6.999   | `gcc -o fib fib.c`               | `time ./fib` |
| C++       | 7.166   | `g++ -o fib fib.ccp`             | `time ./fib` |
| Go        | 6.703   | `go build fib.go`                | `time ./fib` |
| Rust      | 12.038  | `rustc fib.rs`                   | `time ./fib` |
| Swift     | 13.012  | `swiftc -g fib.swift`            | `time ./fib` |
|**Crystal**|**3.857**| `crystal build fib.cr --release` | `time ./fib` |

## VM bytecode, statically typed

| Language      | Time, s | Compile          | Run                 |
|---------------|---------|------------------|---------------------|
| C#            | 43      | `dotnet restore` | `time dotnet run`   |
| C# using Mono | 7.166   | `mcs fib.cs`     | `time mono fib.exe` |
| Java          | 4.672   | `javac Fib.java` | `time java Fib`     |

## Interpreted, dynamically typed

| Language | Time                 | Run                |
|----------|----------------------|--------------------|
| Node     | 12.76 Seconds        | `time node fib.js` |
| Ruby     | 2 Minutes 4 Seconds  | `time ruby fb.rb`  |
| Python   | 5 Minutes 44 Seconds | `time python fb.rb`|
| PHP      | 6 Minutes 2 Seconds  | `time php fb.php`  |

## Caveats

(Fibonacci Benchmark)[https://crystal-lang.org/2016/07/15/fibonacci-benchmark.html]

