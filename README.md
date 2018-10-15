# Recursive Fibonacci Benchmark using top languages on Github

Top 10: JavaScript, Java, Python, Ruby, Php, C++, C#, C, Go [reference](http://www.techworm.net/2016/09/top-10-popular-programming-languages-github.html)

Others: Crystal, Rust, Swift, Mono, Elixir, Perl, R, Julia, D, Nim

This code performs a recursive fibonacci to the 46th position with the result of 2,971,215,073.

Fibonacci can be written many different ways.  The goal of this project is to compare how each language handles the exact same code.

Here is the Ruby version:
```
def fib(n)
  return 1 if n <= 1
  fib(n - 1) + fib(n - 2)
end

puts fib(46)
```

Here is the Crystal version:
```
def fib(n : UInt64)
  return 1_u64 if n <= 1
  fib(n - 1) + fib(n - 2)
end

puts fib(46)
```

All tests are run on:
 - iMac (Retina 5K, 27-inch, Late 2015)
 - Processor: 3.2 GHz Intel Core i5
 - Memory: 16 GB 1867 MHz DDR3
 - OS: macOS Mojave 10.14

Last benchmark was ran on October 15, 2018

## Natively compiled, statically typed

| Language | Time, s  | Compile                                       | Run          |
|----------|----------|-----------------------------------------------|--------------|
| C        |    4.655 | gcc -O3 -o fib fib.c                          | time ./fib   |
| C++      |    4.657 | g++ -O3 -o fib fib.cpp                        | time ./fib   |
| Pony     |    5.577 | ponyc -s -b fib -p ./pony.fib                 | time ./fib   |
| Cython   |    5.903 | cython --embed -o fib.pyx.c fib.pyx && gcc -O3 -o fib fib.pyx.c  | time ./fib   |
| D        |    6.011 | ldc2 -O3 -release -flto=full -of=fib fib.d    | time ./fib   |
| Nim      |    6.022 | nim cpp -d:release fib.nim                    | time ./fib   |
| Rust     |    6.100 | rustc -O fib.rs                               | time ./fib   |
| Fortran  |    6.178 | gfortran -O3 -o fib fib.f03                   | time ./fib   |
| Crystal  |    6.595 | crystal build --release fib.cr                | time ./fib   |
| Swift    |    6.954 | swiftc -O -g fib.swift                        | time ./fib   |
| Haskell  |    8.072 | ghc -O3 -o fib fib.hs                         | time ./fib   |
| OCaml    |    8.097 | ocamlopt -O3 -o fib fib.ml                    | time ./fib   |
| Go       |   10.945 | go build fib.go                               | time ./fib   |
| Lisp     |   13.380 | sbcl --load fib.lisp                          | time ./fib   |

NOTE: 
- Some of these languages perform runtime safety checks while others do not.
- Interesting observation about [code alignment and benchmarks](https://github.com/drujensen/fib/issues/46)

## VM compiled bytecode, statically/dynamically typed

| Language  | Time, s  | Compile                             | Run                       |
|-----------|----------|-------------------------------------|---------------------------|
| Java      |    7.311 | javac Fib.java                      | time java Fib             |
| C#        |   11.176 | dotnet build -c Release -o ./bin    | time dotnet ./bin/fib.dll |
| C# (Mono) |   11.955 | mcs fib.cs                          | time mono fib.exe         |
| Erlang    |   13.170 | erlc +native +'{hipe,[o3]}' fib.erl | time erl -noinput -s fib  |

NOTE: These languages incur a cost for loading the VM that should be taken into consideration when comparing.

## VM compiled before execution, mixed/dynamically typed

| Language  | Time, s  | Run                      |
|-----------|----------|--------------------------|
| Dart      |   10.193 | time dart fib.dart       |
| Escript   |   12.652 | time escript fib.es      |
| Julia     |   13.348 | time julia -O3 fib.jl    |
| Elixir*   |   13.853 | time elixir fib.exs      |
| Node      |   19.277 | time node fib.js         |
| Clojure   |   24.008 | time clojure fib.cljc    |

* Elixir is using ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]'

NOTE: These languages include compilation time that should be taken into consideration when comparing.

## Interpreted, dynamically typed

| Language  | Time, s  | Run                      |
|-----------|----------|--------------------------|
| Scheme    |  150.454 | time guile fib.scm       |
| Php       |  198.279 | time php fib.php         |
| Ruby      |  200.168 | time ruby fib.rb         |
| Lua       |  351.117 | time lua fib.lua         |
| Python    |  515.008 | time python fib.py       |
| Python3   |  770.082 | time python3 fib.py      |
| Perl      | 1039.727 | time perl fib.pl         |
| R         | 1796.495 | time r -f fib.r          |
| Tcl       | 2040.860 | time tclsh fib.tcl       |
| Perl6     | 2851.994 | time perl6 fib.p6        |
| K         |      DNF | time k fib.k             |
| Bash      |      DNF | time bash fib.sh         |

NOTE: Interpreted languages have a startup time cost that should be taken into consideration when comparing.

## Optimized code that breaks the benchmark

The code examples provided in the [optimized](optimized) folder use techniques that break the benchmark. They do not perform the same internal tasks as the other examples so are not a good for an apples to apples comparison. They all perform at sub-second response times. It demonstrates that all benchmarks will have some caveat.

Several of these examples add very little changes to the original code that are worth mentioning:
 - The [C++ constexpr](optimized/fib-constexpr.cpp) is using a `constexpr` which optimizes the recursive call to a constant.
 - The [Lisp compiletime](optimized/fib-compiletime.lisp) shows how you can perform the calculation at compile time. simply add `#.` before outputing the results.
 - The [Python lru_cache](optimized/fib-cache.py) is using `lru_cache` directive with no code changes.
 - The [Ruby mem](optimized/fib-mem.rb) and [JS mem](optimized/fib-mem.js) are maintaining a simple cache to avoid recalculating the results for that value.

For the fun of it, here are the benchmarks for the optimized versions

Last benchmark was ran on October 05, 2018

## Optimized

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| Nim Constant |    0.002 | nim cpp -d:release fib_const.nim | time ./fib_cont |
| Perl Memoized 2 |    0.005 |  | time perl fib-mem2.pl |
| OCaml TCO |    0.005 | ocamlopt -O3 -o fib_tail fib_tail.ml | time ./fib_tail |
| Perl Inline |    0.006 |  | time perl fib-inline.py |
| Go Memoized |    0.006 | go build fib-mem.go | time ./fib-mem |
| Haskell Memoized |    0.006 | ghc -O3 -o fib_mem fib_mem.hs | time ./fib_mem |
| Nim Memoized |    0.006 | nim cpp -d:release fib_mem.nim | time ./fib_mem |
| Swift Memoized |    0.006 | swiftc -O -g fib-mem.swift | time ./fib-mem |
| C++ Constant |    0.007 | g++-8 -O3 -o fib-const fib-constexpr.cpp | time ./fib-const |
| D Memoized |    0.007 | ldc2 -O3 -release -flto=full -of=fib-mem fib-mem.d | time ./fib-mem |
| Lisp Compile Time |    0.008 | sbcl --load fib-compiletime.lisp | time ./fib-compiletime |
| Perl Memoized |    0.014 |  | time perl fib-mem.pl |
| K Memoized |    0.055 |  | time k fib-mem.k |
| Erlang Memoized |    0.113 | erlc +native +'{hipe,[o3]}' fib_mem.erl | time erl -noinput -noshell -s fib_mem |
| Python Cached |    0.116 |  | time python3 fib-cache.py |
| Lua Memoized |    0.121 |  | time lua fib-mem.lua |
| Escript Memoized |    0.136 |  | time escript fib_mem.es |
| Ruby Memoized |    0.183 |  | time ruby fib-mem.rb |
| Java Iterative |    0.199 | javac FibOptimized.java | time java FibOptimized |
| Elixir Iterative |    0.700 |  | time elixir fib-iterative.exs |
| Node Memoized |    0.835 |  | time node fib-mem.js |
| Perl6 Memoized |    1.005 |  | time perl6 fib-mem.p6 |
| Lisp Local |    7.550 | sbcl --load fib-local.lisp | time ./fib-local |

## Versions

- g++ Apple LLVM version 10.0.0 (clang-1000.11.45.2)
- gcc Apple LLVM version 10.0.0 (clang-1000.11.45.2)
- nim Nim Compiler Version 0.18.0 [MacOSX: amd64]
- crystal Crystal 0.26.1 (2018-08-27) LLVM: 6.0.1
- cython Cython version 0.28.5
- gfortan GNU Fortran (Homebrew GCC 8.2.0) 8.2.0
- rustc 1.29.0
- lcd2 the LLVM D compiler (1.11.0)
- swiftc Apple Swift version 4.2 (swiftlang-1000.11.37.1 clang-1000.11.45.1)
- ghc The Glorious Glasgow Haskell Compilation System, version 8.4.3
- ocaml The OCaml toplevel, version 4.07.0
- go version go1.11 darwin/amd64
- lisp SBCL 1.4.11
- javac 10.0.1
- dotnet 2.1.4
- mcs Mono C# compiler version 5.12.0.226
- erlc 21.1
- dart Dart VM version: 2.0.0 (Fri Aug 3 10:53:23 2018 +0200)
- julia version 0.6.3
- elixir Elixir 1.7.3 (compiled with Erlang/OTP 21)
- node v9.4.0
- clojure 1.9.0
- guile (GNU Guile) 2.2.4
- php 7.1.16 (cli) (built: Apr  1 2018 13:14:42)
- ruby 2.5.1p57 (2018-03-29 revision 63029)
- python 2.7.15
- python3 3.7.0
- perl 5, version 26, subversion 2 (v5.26.2)
- perl 6 This is Rakudo Star version 2018.06 built on MoarVM version 2018.06
- lua Lua 5.3.5  Copyright (C) 1994-2018 Lua.org, PUC-Rio
- r version 3.5.0 (2018-04-23)
- lcd2 the LLVM D compiler (1.11.0)
- dmd the reference D compiler (2.082.0)
- ocaml The OCaml toplevel, version 4.07.0
- ghc The Glorious Glasgow Haskell Compilation System, version 8.4.3
- gfortran GNU Fortran (Homebrew GCC 8.2.0) 8.2.0
- ponyc 0.25.0 compiled with: llvm 3.9.1
- tchsh 8.5

## Caveats

[Fibonacci Benchmark](https://crystal-lang.org/2016/07/15/fibonacci-benchmark.html)
