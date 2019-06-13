# Recursive Fibonacci Benchmark using top languages on Github

Top 10: JavaScript, Java, Python, Ruby, Php, C++, C#, C, Go [reference](http://www.techworm.net/2016/09/top-10-popular-programming-languages-github.html)

Others: Crystal, Rust, Swift, Mono, Elixir, Perl, R, Julia, D, Nim, Pascal, Fortran, Cython, Pony, OCaml, Lisp, Haskell, Erlang, Escript, Dart, Clojure, Scheme, Lua, Python3, Perl, Perl6, Bash, Emoji

The code performs a recursive fibonacci to the 46th position with the result of 2,971,215,073.  This is the original version where the sequence starts at 1 instead of 0. 1,1,2,3,5,8...

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
 - MacBook Pro (Retina, 15-inch, Mid 2015)
 - Processor: 2.5 GHz Intel Core i7
 - Memory: 16 GB 1600 MHz DDR3
 - OS: macOS Mojave 10.14.5
 - Docker Container: 2.0.0.3
 - Docker Image: ubuntu:18.04

You can run the tests using Docker: `docker run -it drujensen/fib`

Last benchmark was ran on June 12, 2019

## Natively compiled, statically typed

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| Nim |    0.429 | nim cpp -d:release fib.nim | time ./fib |
| C++ |    3.856 | g++ -O3 -o fib fib.cpp | time ./fib |
| C |    3.881 | gcc -O3 -o fib fib.c | time ./fib |
| Fortran |    3.889 | gfortran -O3 -o fib fib.f03 | time ./fib |
| Cython |    4.147 | cython --embed -o fib.pyx.c fib.pyx && gcc -O3 -o fib fib.pyx.c | time ./fib |
| D |    5.274 | ldc2 -O3 -release -flto=full -of=fib fib.d | time ./fib |
| Pony |    5.806 | ponyc -s -b fib -p ./fib.pony | time ./fib |
| Rust |    5.812 | rustc -C opt-level=s fib.rs | time ./fib |
| Crystal |    5.817 | crystal build --release fib.cr | time ./fib |
| Swift |    6.483 | swiftc -O -g fib.swift | time ./fib |
| OCaml |    7.831 | ocamlopt -O3 -o fib fib.ml | time ./fib |
| Pascal |    9.269 | fpc -O3 ./fib.pas | time ./fib |
| Go |   11.390 | go build fib.go | time ./fib |
| Lisp |   13.290 | sbcl --load fib.lisp | time ./fib |
| Haskell |   23.700 | ghc -O3 -o fib fib.hs | time ./fib |

- Why is Nim is so fast? [https://forum.nim-lang.org/t/4253](https://forum.nim-lang.org/t/4253)

## VM compiled bytecode, statically typed

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| Java |    7.070 | javac Fib.java | time java Fib |
| C# |   10.628 | dotnet build -c Release -o ./bin | time dotnet ./bin/fib.dll |
| C# (Mono) |   13.809 | mcs Fib.cs | time mono Fib.exe |
| Erlang |   14.213 | erlc +native +'{hipe,[o3]}' fib.erl | time erl -noinput -noshell -s fib |

## VM compiled before execution, mixed/dynamically typed

| Language | Time, s | Run |
|----------|---------|-----|
| Dart |    9.716 | time dart fib.dart |
| Julia |   10.716 | time julia -O3 fib.jl |
| Escript |   14.647 | time escript fib.es |
| Lua Jit |   15.470 | time luajit fib.lua |
| Node |   22.036 | time node fib.js |
| Elixir |    23.190 | time elixir fib.exs |
| Clojure |    27.719 | time clojure fib.cljc |

- Elixir is using ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]'

NOTE: These languages include compilation time which should be taken into consideration when comparing.

## Interpreted, dynamically typed

| Language | Time, s | Run |
|----------|---------|-----|
| Php |  161.610 | time php fib.php |
| Scheme | 221.163 | time guile fib.scm |
| Ruby |  260.350 | time ruby fib.rb |
| Lua |  460.107 | time lua fib.lua |
| Python3 |  657.607 | time python3 fib.py |
| Python |  675.068 | time python fib.py |
| Perl | 1663.980 | time perl fib.pl |
| Tcl | 1957.335 | time tclsh fib.tcl |
| Perl 6 | 5419.500 | time perl6 fib.p6 |
| K         |      DNF | time k fib.k |
| R         |      DNF | time r -f fib.r |
| Bash      |      DNF | time bash fib.sh |
| PowerShell |     DNF | time pwsh fib.ps1 |

NOTE: Interpreted languages have a startup time cost that should be taken into consideration when comparing.

## Optimized code that breaks the benchmark

The code examples provided in the [optimized](optimized) folder use techniques that break the benchmark. They do not perform the same internal tasks as the other examples so are not a good for an apples to apples comparison. They all perform at sub-second response times. It demonstrates that all benchmarks will have some caveat.

Several of these examples add very little changes to the original code that are worth mentioning:
 - The [C++ constexpr](optimized/fib-constexpr.cpp) is using a `constexpr` which optimizes the recursive call to a constant.
 - The [Lisp compiletime](optimized/fib-compiletime.lisp) shows how you can perform the calculation at compile time. simply add `#.` before outputing the results.
 - The [Python lru_cache](optimized/fib-cache.py) is using `lru_cache` directive with no code changes.
 - The [Ruby mem](optimized/fib-mem.rb), [Crystal mem](optimized/fib-mem.cr) and [JS mem](optimized/fib-mem.js) are maintaining a simple cache to avoid recalculating the results for that value.

For the fun of it, here are the benchmarks for the optimized versions

Last benchmark was ran on November 30, 2018

## Optimized

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| Nim Constant |    0.003 | nim cpp -d:release fib_const.nim | time ./fib_cont |
| Crystal Memoized |    0.004 | crystal build --release fib-mem.cr | time ./fib-mem |
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

All compilers are installed using `asdf` or `apt` on Ubuntu 18.04 docker image:
- gcc         7.4.0
- g++         7.4.0
- java        openjdk-11.0.1
- ruby        2.6.3
- clojure     1.10.0
- crystal     0.29.0
- dart        2.2.0
- dotnet-core 2.2.105
- erlang      22.0.2
- elixir      1.8.2
- elm         0.19.0
- golang      1.12.5
- haskell     8.6.5
- julia       1.1.1
- lua         5.3.5
- luajit      2.1.0-beta3
- nim         v0.20.0
- nodejs      8.12.0
- pascal      3.0.4
- perl        5.28.1
- php         7.3.6
- python      2.7.16
- python3     3.7.3
- rust        1.35.0
- R           3.6.0
- D (lcd2)    1.15.1
- ponyc       0.28.1
- swift       5.0.1
- cython      0.26.1
- gfortran    7.4.0
- ocaml       4.07.1
- perl6       2018.06
- lisp (sbcl) 1.4.5
- tcl         8.6
- scheme      2.2

## Caveats

[Fibonacci Benchmark](https://crystal-lang.org/2016/07/15/fibonacci-benchmark.html)
