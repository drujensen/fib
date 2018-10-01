# Recursive Fibonacci Benchmark using top languages on Github

Top 10: JavaScript, Java, Python, Ruby, Php, C++, C#, C, Go [reference](http://www.techworm.net/2016/09/top-10-popular-programming-languages-github.html)

Others: Crystal, Rust, Swift, Mono, Elixir, Perl, R, Julia, D, Nim

This code performs a recursive fibonacci to the 46th position with the result of 2,971,215,073.

Fibonacci can be written many different ways.  The goal of this project is to compare how each language handles the exact same code.

Here is the Crystal version:
```
def fib(n : UInt64)
  return 1_u64 if n <= 1
  fib(n - 1) + fib(n - 2)
end

puts fib(46)
```

Here is the Ruby version:
```
def fib(n)
  return 1 if n <= 1
  fib(n - 1) + fib(n - 2)
end

puts fib(46)
```

All tests are run on:
 - iMac (Retina 5K, 27-inch, Late 2015)
 - OS: macOS High Sierra 10.13.6
 - Processor: 3.2 GHz Intel Core i5
 - Memory: 16 GB 1867 MHz DDR3

Last benchmark was ran on September 29, 2018

## Natively compiled, statically typed

| Language   | Time, s   | Compile                                      | Run          |
|------------|-----------|----------------------------------------------|--------------|
| Nim        |    4.678 | nim cpp -d:release fib.nim                    | time ./fib   |
| C          |    5.418 | gcc -O3 -o fib fib.c                          | time ./fib   |
| C++        |    5.456 | g++ -O3 -o fib fib.cpp                        | time ./fib   |
| Crystal    |    5.800 | crystal build --release fib.cr                | time ./fib   |
| Fortran    |    6.139 | gfortran -O3 -o fib fib.f03                   | time ./fib   |
| Rust       |    6.724 | rustc -O fib.rs                               | time ./fib   |
| D          |    7.124 | ldc2 -O3 -release -flto=full -of=fib fib.d    | time ./fib   |
| OCaml      |    8.131 | ocamlopt -O3 -o fib fib.ml                    | time ./fib   |
| Haskell    |    8.143 | ghc -O3 -o fib fib.hs                         | time ./fib   |
| Swift      |   10.550 | swiftc -O -g fib.swift                        | time ./fib   |
| Go         |   10.863 | go build fib.go                               | time ./fib   |
| Cython     |          | `cython3 --embed -o fib.pyx.c fib.pyx &&`<br>`gcc -O3 -o fib fib.pyx.c $(pkg-config --cflags --libs python3)` | `time ./fib` |

NOTE: C and C++ compile to the exact same machine code but the C version is slower because it doesn't run at the same address in the processor.  Thanks @glandium for pointing this out. See [Issue #46](https://github.com/drujensen/fib/issues/46)

## VM compiled bytecode, statically typed

| Language  | Time, s | Compile                            | Run                         |
|-----------|---------|------------------------------------|-----------------------------|
| Java      |    7.556 | javac Fib.java                       | time java Fib               |
| C#        |   11.387 | dotnet build -c Release -o ./bin     | time dotnet ./bin/fib.dll   |
| C# (Mono) |   12.310 | mcs fib.cs                           | time mono fib.exe           |

## VM compiled, dynamically typed

| Language  | Time, s | Compile                               | Run                                 |
|-----------|---------|---------------------------------------|-------------------------------------|
| Erlang    |         | `erlc +native +'{hipe,[o3]}' fib.erl` | `time erl -noimput -noshell -s fib` |

## VM compiled before execution, mixed/dynamically typed

| Language  | Time, s | Run                         |
|-----------|---------|-----------------------------|
| Dart      |    9.651 | time dart fib.dart         |
| Julia     |   11.461 | time julia -O3 fib.jl      |
| Elixir*   |   13.955 | time elixir fib.exs        |
| Node      |   19.161 | time node fib.js           |
| Escript(Erlang) |          | `time escript fib.es`|

* Elixir is using ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]'

NOTE: These languages include compilation time that should be taken into consideration when comparing.

## Interpreted, dynamically typed

| Language  | Time, s | Run                         |
|-----------|---------|-----------------------------|
| Php       |  198.279 | time php fib.php           |
| Ruby      |  202.901 | time ruby fib.rb           |
| Scheme    |  ??????? | `time guile fib.scm`       |
| Python    |  512.621 | time python fib.py         |
| Python3   |  758.681 | `time python3 fib.py`      |
| Perl      | 1133.131 | `time perl fib.pl`         |
| Perl 6    |     TODO | `time perl6 fib.p6`        |
| Tcl       |     TODO | `time tclsh fib.tcl`       |
| Lua       |     TODO | `time lua fib.lua`         |
| R         | 1796.495 | `time r -f fib.r`          |
| K         |     TODO | `time k fib.k`             |

NOTE: Interpreted languages have a startup time cost that should be taken into consideration when comparing.

## Optimized code that breaks the benchmark

The code examples provided in the [optimized](optimized) folder use techniques that break the benchmark. They do not perform the same internal tasks as the other examples so are not a good for an apples to apples comparison. They all perform at sub-second response times. It demonstrates that all benchmarks will have some caveat.

Several of these examples add very little changes to the original code:
 - The [C++ constexpr](optimized/fib-constexpr.cpp) is using a `constexpr` which optimizes the recursive call to a constant.
 - The [Python lru_cache](optimized/fib-cache.py) is using `lru_cache` directive with no code changes.
 - The [Ruby mem](optimized/fib-mem.rb) and [JS mem](optimized/fib-mem.js) are maintaining a simple cache in an array.
| D (mem)         |          | `dmd -O -release fibmem.d`           | `time ./fib`           |


## Versions

- go version go1.11 darwin/amd64
- g++-8 (Homebrew GCC 8.2.0) 8.2.0
- crystal Crystal 0.26.1 (2018-08-27) LLVM: 6.0.1
- g++ Apple LLVM version 10.0.0 (clang-1000.11.45.2)
- gcc Apple LLVM version 10.0.0 (clang-1000.11.45.2)
- nim Nim Compiler Version 0.18.0 [MacOSX: amd64]
- swiftc Apple Swift version 4.2 (swiftlang-1000.11.37.1 clang-1000.11.45.1)
- rustc 1.29.0
- javac 10.0.1
- mcs Mono C# compiler version 5.12.0.226
- dotnet 2.1.4
- dart Dart VM version: 2.0.0 (Fri Aug 3 10:53:23 2018 +0200)
- julia version 0.6.3
- node v9.4.0
- elixir Elixir 1.7.3 (compiled with Erlang/OTP 21)
- ruby 2.5.1p57 (2018-03-29 revision 63029)
- php 7.1.16 (cli) (built: Apr  1 2018 13:14:42)
- python 2.7.15
- python3 3.7.0
- perl 5, version 26, subversion 2 (v5.26.2)
- perl6 This is Rakudo Star version 2018.06 built on MoarVM version 2018.06
- r version 3.5.0 (2018-04-23)
- lcd2 the LLVM D compiler (1.11.0)
- dmd the reference D compiler (2.082.0)
- ocaml The OCaml toplevel, version 4.07.0
- ghc The Glorious Glasgow Haskell Compilation System, version 8.4.3
- gfortran GNU Fortran (Homebrew GCC 8.2.0) 8.2.0
- tchsh 8.5

## Caveats

[Fibonacci Benchmark](https://crystal-lang.org/2016/07/15/fibonacci-benchmark.html)
