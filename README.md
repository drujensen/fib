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

Last benchmark was ran on October 17, 2018

## Natively compiled, statically typed

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| C++ |    4.559 | g++ -O3 -o fib fib.cpp | time ./fib |
| C |    4.582 | gcc -O3 -o fib fib.c | time ./fib |
| Nim |    5.868 | nim cpp -d:release fib.nim | time ./fib |
| D |    5.908 | ldc2 -O3 -release -flto=full -of=fib fib.d | time ./fib |
| Cython |    5.921 | cython --embed -o fib.pyx.c fib.pyx && gcc -O3 -o fib fib.pyx.c | time ./fib |
| Fortran |    5.983 | gfortran -O3 -o fib fib.f03 | time ./fib |
| Pony |    6.034 | ponyc -s -b fib -p ./pony.fib | time ./fib |
| Crystal |    6.486 | crystal build --release fib.cr | time ./fib |
| Rust |    6.509 | rustc -C opt-level=s fib.rs | time ./fib |
| Swift |    6.783 | swiftc -O -g fib.swift | time ./fib |
| OCaml |    7.916 | ocamlopt -O3 -o fib fib.ml | time ./fib |
| Haskell |    7.921 | ghc -O3 -o fib fib.hs | time ./fib |
| Go |   10.635 | go build fib.go | time ./fib |
| Lisp |   13.178 | sbcl --load fib.lisp | time ./fib |

NOTE: 
- Some of these languages perform runtime safety checks while others do not.
- Interesting observation about [code alignment and benchmarks](https://github.com/drujensen/fib/issues/46)

## VM compiled bytecode, statically/dynamically typed

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| Java |    7.383 | javac Fib.java | time java Fib |
| C# (Mono) |   11.124 | mcs fib.cs | time mono fib.exe |
| C# |   11.234 | dotnet build -c Release -o ./bin | time dotnet ./bin/fib.dll |
| Erlang |   13.183 | erlc +native +'{hipe,[o3]}' fib.erl | time erl -noinput -noshell -s fib |

NOTE: These languages incur a cost for loading the VM that should be taken into consideration when comparing.

## VM compiled before execution, mixed/dynamically typed

| Language | Time, s | Run |
|----------|---------|-----|
| Julia |    8.530 | time julia -O3 fib.jl |
| Dart |    9.209 | time dart fib.dart |
| Escript |   12.792 | time escript fib.es |
| Node |   19.124 | time node fib.js |
| Elixir |   21.642 | time elixir fib.exs |
| Clojure |   24.835 | time clojure fib.cljc |

* Elixir is using ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]'

NOTE: These languages include compilation time that should be taken into consideration when comparing.

## Interpreted, dynamically typed

| Language | Time, s | Run |
|----------|---------|-----|
| Scheme |  147.859 | time guile fib.scm |
| Ruby |  192.207 | time ruby fib.rb |
| Php |  195.468 | time php fib.php |
| Lua |  276.998 | time lua fib.lua |
| Python |  502.276 | time python fib.py |
| Python3 |  751.082 | time python3 fib.py |
| Perl | 1014.648 | time perl fib.pl |
| Tcl | 1517.857 | time tclsh fib.tcl |
| Perl 6 | 2894.135 | time perl6 fib.p6 |
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

Last benchmark was ran on October 15, 2018

## Optimized

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| Nim Constant |    0.003 | nim cpp -d:release fib_const.nim | time ./fib_cont |
| Perl Memoized 2 |    0.005 |  | time perl fib-mem2.pl |
| OCaml TCO |    0.005 | ocamlopt -O3 -o fib_tail fib_tail.ml | time ./fib_tail |
| Haskell Memoized |    0.005 | ghc -O3 -o fib_mem fib_mem.hs | time ./fib_mem |
| Nim Memoized |    0.005 | nim cpp -d:release fib_mem.nim | time ./fib_mem |
| Swift Memoized |    0.006 | swiftc -O -g fib-mem.swift | time ./fib-mem |
| C++ Constant |    0.006 | g++-8 -O3 -o fib-const fib-constexpr.cpp | time ./fib-const |
| Go Memoized |    0.006 | go build fib-mem.go | time ./fib-mem |
| Perl Inline |    0.007 |  | time perl fib-inline.py |
| D Memoized |    0.007 | ldc2 -O3 -release -flto=full -of=fib-mem fib-mem.d | time ./fib-mem |
| Lisp Compile Time |    0.008 | sbcl --load fib-compiletime.lisp | time ./fib-compiletime |
| Perl Memoized |    0.015 |  | time perl fib-mem.pl |
| Python Cached |    0.034 |  | time python3 fib-cache.py |
| Lua Memoized |    0.040 |  | time lua fib-mem.lua |
| Node Memoized |    0.065 |  | time node fib-mem.js |
| Ruby Memoized |    0.081 |  | time ruby fib-mem.rb |
| Erlang Memoized |    0.104 | erlc +native +'{hipe,[o3]}' fib_mem.erl | time erl -noinput -noshell -s fib_mem |
| Escript Memoized |    0.139 |  | time escript fib_mem.es |
| K Memoized |    0.220 |  | time k fib-mem.k |
| Java Iterative |    0.233 | javac FibOptimized.java | time java FibOptimized |
| Elixir Iterative |    0.461 |  | time elixir fib-iterative.exs |
| Perl6 Memoized |    0.915 |  | time perl6 fib-mem.p6 |

## Versions

- g++ Apple LLVM version 10.0.0 (clang-1000.11.45.2)
- gcc Apple LLVM version 10.0.0 (clang-1000.11.45.2)
- nim Nim Compiler Version 0.19.0 [MacOSX: amd64]
- crystal Crystal 0.26.1 (2018-09-26) LLVM: 6.0.1
- cython Cython version 0.28.5
- gfortan GNU Fortran (Homebrew GCC 8.2.0) 8.2.0
- rustc 1.29.2
- ldc2 the LLVM D compiler (1.11.0)
- swiftc Apple Swift version 4.2 (swiftlang-1000.11.37.1 clang-1000.11.45.1)
- ghc The Glorious Glasgow Haskell Compilation System, version 8.4.3
- ocaml The OCaml toplevel, version 4.07.0
- go version go1.11.1 darwin/amd64
- sbcl Lisp SBCL 1.4.12
- javac 10.0.1
- dotnet 2.1.4
- mcs Mono JIT compiler version 5.14.0.177
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
