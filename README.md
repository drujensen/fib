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
 - MacBook Pro (16-inch, 2019)
 - Processor: 2.6 GHz 6-Core Intel Core i7
 - Memory: 32 GB 2667 MHz DDR4
 - OS: macOS Mojave 10.15.5
 - Docker Version: 19.03.8
 - Docker Image: ubuntu:18.04

You can run the tests using Docker: `docker run -it drujensen/fib`

Last benchmark was ran on June 21, 2020

## Natively compiled, statically typed

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| Swift |    4.298 | swiftc -O -g fib.swift | ./fib |
| Fortran |    4.605 | gfortran -fno-inline-small-functions -O3 -o fib fib.f03 | ./fib |
| C |    4.760 | gcc -fno-inline-small-functions -O3 -o fib fib.c | ./fib |
| C++ |    4.771 | g++ -fno-inline-small-functions -O3 -o fib fib.cpp | ./fib |
| Pony |    4.796 | ponyc -s -b fib -p ./fib.pony | ./fib |
| V |    5.067 | v -cflags -fno-inline-small-functions -prod -o fib fib.v | ./fib |
| D |    5.078 | bash -c "ldc2 -O3 -release -flto=full -of=fib fib.d" | ./fib |
| Nim |    5.087 | nim cpp -d:release --passC:-fno-inline-small-functions fib.nim | ./fib |
| Cython |    5.289 | cython --embed -o fib.pyx.c fib.pyx && gcc -fno-inline-small-functions -O3 -o fib fib.pyx.c $(pkg-config --cflags --libs python) | ./fib |
| Rust |    5.450 | rustc -C opt-level=3 -C lto=fat fib.rs | ./fib |
| Pascal |    6.544 | fpc -O3 -Si ./fib.pas | ./fib |
| Assembly |    7.993 | gcc -no-pie -o fib fib-gcc-x64.s | ./fib |
| OCaml |    8.607 | ocamlopt -O3 -o fib fib.ml | ./fib |
| Crystal |    8.945 | crystal build --release fib.cr | ./fib |
| Go |    9.717 | go build fib.go | ./fib |
| Lisp |   12.186 | sbcl --load fib.lisp | ./fib |
| Haskell |   26.685 | rm ./fib.o && ghc -O3 -o fib fib.hs | ./fib |

## VM compiled bytecode, statically typed

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| Java |    6.279 | javac Fib.java | java Fib |
| Kotlin |    6.285 | kotlinc Fib.kt -include-runtime -d Fib.jar | java -jar Fib.jar |
| C# |   11.138 | dotnet build -c Release -o ./bin | dotnet ./bin/fib.dll |
| C# (Mono) |   12.673 | mcs Fib.cs | mono Fib.exe |
| Erlang |   25.166 | erlc +native +'{hipe,[o3]}' fib.erl | erl -noinput -noshell -s fib |

## VM compiled before execution, mixed/dynamically typed

| Language | Time, s | Run |
|----------|---------|-----|
| Dart |    8.606 | dart fib.dart |
| Julia |    8.767 | julia -O3 fib.jl |
| Lua Jit |   13.028 | luajit fib.lua |
| Node |   20.048 | node fib.js |
| Clojure |    27.114 | clojure fib.cljc |
| Escript |   24.966 | escript fib.es |
| Elixir |    94.414 | ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]' && elixir fib.exs |

NOTE: These languages include compilation time which should be taken into consideration when comparing.

## Interpreted, dynamically typed

| Language | Time, s | Run |
|----------|---------|-----|
| Php |  111.203 | php fib.php |
| Scheme |  250.139 | guile fib.scm |
| Ruby |  283.398 | ruby fib.rb |
| Lua |    495.116 | lua fib.lua |
| Python3 |  598.509 | python3 fib.py |
| Janet |  607.709 | janet ./fib.janet |
| Python |  797.384 | python fib.py |
| Perl | 1720.586 | perl fib.pl |
| Tcl | 1793.580 | tclsh fib.tcl |
| Perl 6 | 5342.897 | perl6 fib.p6 |
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

Last benchmark was ran on June 21, 2020

## Optimized

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| C++ Constant |    0.000 | g++-8 -O3 -o fib-const fib-constexpr.cpp | ./fib-const |
| Elixir Iterative |    0.000 |  | elixir fib-iterative.exs |
| Go Memoized |    0.001 | go build fib-mem.go | ./fib-mem |
| K Memoized |    0.001 |  | k fib-mem.k |
| Lua Memoized |    0.001 |  | lua fib-mem.lua |
| Nim Constant |    0.001 | nim cpp -d:release fib_const.nim | ./fib_cont |
| OCaml TCO |    0.001 | ocamlopt -O3 -o fib_tail fib_tail.ml | ./fib_tail |
| Nim Memoized |    0.002 | nim cpp -d:release fib_mem.nim | ./fib_mem |
| Nim Rewrite |    0.002 | nim c -d:release fib_rewrite.nim | ./fib_rewrite |
| Perl Memoized 2 |    0.002 |  | perl fib-mem2.pl |
| Perl Inline |    0.002 |  | perl fib-inline.py |
| Haskell Memoized |    0.002 | ghc -O3 -o fib_mem fib_mem.hs | ./fib_mem |
| Swift Memoized |    0.003 | swiftc -O -g fib-mem.swift | ./fib-mem |
| D Memoized |    0.004 | ldc2 -O3 -release -flto=full -of=fib-mem fib-mem.d | ./fib-mem |
| Tcl Memoized |    0.004 |  | tclsh fib-mem.tcl |
| Janet Memoized |    0.004 |  | janet ./fib-mem.janet |
| Janet TCO |    0.004 |  | janet ./fib-tco.janet |
| Perl Memoized |    0.009 |  | perl fib-mem.pl |
| Python Cached |    0.018 |  | python3 fib-cache.py |
| Lisp Compile Time |    0.019 | sbcl --load fib-compiletime.lisp | ./fib-compiletime |
| Ruby Memoized |    0.051 |  | ruby fib-mem.rb |
| Node Memoized |    0.247 |  | node fib-mem.js |
| Java Iterative |    0.265 | javac FibOptimized.java | java FibOptimized |
| Perl6 Memoized |    0.305 |  | perl6 fib-mem.p6 |
| Erlang Memoized |    0.729 | erlc +native +'{hipe,[o3]}' fib_mem.erl | erl -noinput -noshell -s fib_mem |
| Escript Memoized |    0.776 |  | escript fib_mem.es |

## Versions

All compilers are installed using `asdf` or `apt` on Ubuntu 18.04 docker image:
assembly (gcc) 7.4.0
bash           5.0.0
crystal        0.35.0
clojure        1.10.1
cython         0.29.15
D (lcd2)       v1.20.1
dart           2.7.2
dotnet-core    3.1.201
elixir         1.10.0
elm            0.19.1
erlang         22.3.1
g++            7.4.0
gcc            7.4.0
gfortran       9.3.0
golang         1.14.1
haskell        8.10.1
java           adopt-openjdk-14+36
julia          1.4.0
K              3.6
lisp (sbcl)    2.0.3
lua            5.3.0
mono           6.8.0
nim            v1.2.0
nodejs         12.16.1
ocaml          4.10.0
pascal         3.0.4
perl           5.30.2
php            7.4.4
ponyc          0.33.2
powershell     v7.0.0
python         3.8.2
R              3.6.3
rakudo         2020.01
ruby           2.7.1
rust           1.42.0
scheme (guile) 2.2
swift          5.2.1
tcl            8.6.10

## Caveats

[Fibonacci Benchmark](https://crystal-lang.org/2016/07/15/fibonacci-benchmark.html)
