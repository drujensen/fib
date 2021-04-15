# Recursive Fibonacci Benchmark using top languages on Github

Top 10: JavaScript, Java, Python, Ruby, Php, C++, C#, C, Go [reference](http://www.techworm.net/2016/09/top-10-popular-programming-languages-github.html)

Others: Crystal, Rust, Swift, Mono, Elixir, Perl, R, Julia, D, Nim, Pascal, Fortran, Cython, Pony, OCaml, Lisp, Haskell, Erlang, Escript, Dart, Clojure, Scheme, Lua, Python3, PyPy, Perl, Perl6, Bash, Emoji

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
  return 1 if n <= 1
  fib(n - 1) + fib(n - 2)
end

puts fib(46)
```

All tests are run on:
 - AWS EC2 - m5.large 2 vCPU
 - Processor: Intel Xeon 3.1Ghz
 - Memory: 8 GiB
 - OS: Ubuntu 20.04
 - Docker Base Image: ubuntu:20.04

You can run the tests using Docker: `docker run -it drujensen/fib`

Last benchmark was ran on April 15, 2021

## Natively compiled, statically typed

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| V |    5.709 | v -cflags -fno-inline-small-functions -prod -o fib fib.v | ./fib |
| Pony |    5.820 | ponyc -s -b fib -p ./fib.pony | ./fib |
| C++ |    5.876 | g++ -fno-inline-small-functions -O3 -o fib fib.cpp | ./fib |
| C |    5.878 | gcc -fno-inline-small-functions -O3 -o fib fib.c | ./fib |
| D |    5.901 | bash -c "ldc2 -O3 -release -flto=full -of=fib fib.d" | ./fib |
| Fortran |    6.027 | gfortran -fno-inline-small-functions -O3 -o fib fib.f03 | ./fib |
| Crystal |    6.136 | crystal build --release fib.cr | ./fib |
| Rust |    6.376 | rustc -C opt-level=3 fib.rs | ./fib |
| Nim |    6.424 | nim c -d:danger --passC:-fno-inline-small-functions fib.nim | ./fib |
| Ada |    6.978 | gnat make -O2 -gnatp -o fib fib.adb | ./fib |
| Cython |    6.984 | cython -3 --embed -o fib.pyx.c fib.pyx && gcc -fno-inline-small-functions -O3 -o fib fib.pyx.c $(pkg-config --cflags --libs python) | ./fib |
| Swift |    7.524 | swiftc -O -g fib.swift | ./fib |
| Pascal |    8.353 | fpc -O3 -Si ./fib.pas | ./fib |
| OCaml |    9.865 | ocamlopt -O3 -o fib fib.ml | ./fib |
| Assembly |    9.981 | gcc -no-pie -o fib fib-gcc-x64.s | ./fib |
| Go |   12.469 | go build fib.go | ./fib |
| Haskell |   13.277 | rm ./fib.o && ghc -O3 -o fib fib.hs | ./fib |
| Lisp |   15.516 | sbcl --load fib.lisp | ./fib |

NOTE: DSB Boundary 64 byte alignment may affect results.  See [issue #129](https://github.com/drujensen/fib/issues/129) for details.

## VM compiled bytecode, statically typed

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| Kotlin |    8.168 | kotlinc Fib.kt -include-runtime -d Fib.jar | java -jar Fib.jar |
| Java |    8.303 | javac Fib.java | java Fib |
| C# |   13.181 | dotnet build -c Release -o ./bin | dotnet ./bin/fib.dll |
| C# (Mono) |   15.706 | mcs Fib.cs | mono Fib.exe |
| Erlang |   16.502 | erlc +native +'{hipe,[o3]}' fib.erl | erl -noinput -noshell -s fib |

## VM compiled before execution, mixed/dynamically typed

| Language | Time, s | Run |
|----------|---------|-----|
| Julia |   11.182 | julia -O3 fib.jl |
| Dart |   11.531 | dart fib.dart |
| Escript |   16.066 | escript fib.es |
| Lua Jit |   16.633 | luajit fib.lua |
| Elixir |   17.099 | ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]' elixir Fib.exs |
| Node |   25.975 | node fib.js |
| Clojure |   35.140 | clojure -M fib.cljc |
| Python3 (PyPy) |   40.708 | pypy3 fib.py |
| Ruby (jit) |   62.792 | ruby --jit fib.rb |

NOTE: These languages include compilation time which should be taken into consideration when comparing.

## Interpreted, dynamically typed

| Language | Time, s | Run |
|----------|---------|-----|
| Php |  117.779 | php fib.php |
| Ruby |  187.516 | ruby fib.rb |
| Scheme |  191.774 | guile fib.scm |
| Lua |  262.031 | lua fib.lua |
| Janet |  296.435 | janet ./fib.janet |
| Python |  508.441 | python fib.py |
| Python3 |  531.542 | python3 fib.py |
| Perl | 1101.333 | perl fib.pl |
| Tcl | 1620.024 | tclsh fib.tcl |
| Perl 6 | 3174.896 | perl6 fib.p6 |
| K |    DNF | k fib.k |
| R |    DNF | r -f fib.r |
| Bash |    DNF | bash fib.sh |
| Powershell |    DNF | pwsh fib.ps1 |

NOTE: Interpreted languages have a startup time cost that should be taken into consideration when comparing.

## Optimized code that breaks the benchmark

The code examples provided in the [optimized](optimized) folder use techniques that break the benchmark. They do not perform the same internal tasks as the other examples so are not a good for an apples to apples comparison. They all perform at sub-second response times. It demonstrates that all benchmarks will have some caveat.

Several of these examples add very little changes to the original code that are worth mentioning:
 - The [C++ constexpr](optimized/fib-constexpr.cpp) is using a `constexpr` which optimizes the recursive call to a constant.
 - The [Lisp compiletime](optimized/fib-compiletime.lisp) shows how you can perform the calculation at compile time. simply add `#.` before outputing the results.
 - The [Python lru_cache](optimized/fib-cache.py) is using `lru_cache` directive with no code changes.
 - The [Ruby mem](optimized/fib-mem.rb), [Crystal mem](optimized/fib-mem.cr) and [JS mem](optimized/fib-mem.js) are maintaining a simple cache to avoid recalculating the results for that value.

For the fun of it, here are the benchmarks for the optimized versions

Last benchmark was ran on April 15, 2021

## Optimized

| Language | Time, s | Compile | Run |
|----------|---------|---------|-----|
| C++ Constant |    0.000 | g++-8 -O3 -o fib-const fib-constexpr.cpp | ./fib-const |
| Nim Constant |    0.000 | nim c -d:danger fib_const.nim | ./fib_cont |
| Lisp Compile Time |    0.000 | sbcl --load fib-compiletime.lisp | ./fib-compiletime |
| Perl Memoized 2 |    0.000 |  | perl fib-mem2.pl |
| D Memoized |    0.001 | ldc2 -O3 -release -flto=full -of=fib-mem fib-mem.d | ./fib-mem |
| Go Memoized |    0.001 | go build fib-mem.go | ./fib-mem |
| K Memoized |    0.001 |  | k fib-mem.k |
| Nim Memoized |    0.001 | nim c -d:danger fib_mem.nim | ./fib_mem |
| Nim Rewrite |    0.001 | nim c -d:danger fib_rewrite.nim | ./fib_rewrite |
| Perl Inline |    0.001 |  | perl fib-inline.py |
| Swift Memoized |    0.001 | swiftc -O -g fib-mem.swift | ./fib-mem |
| Haskell Memoized |    0.001 | ghc -O3 -o fib_mem fib_mem.hs | ./fib_mem |
| OCaml TCO |    0.001 | ocamlopt -O3 -o fib_tail fib_tail.ml | ./fib_tail |
| Janet Memoized |    0.002 |  | janet ./fib-mem.janet |
| Janet TCO |    0.002 |  | janet ./fib-tco.janet |
| Tcl Memoized |    0.003 |  | tclsh fib-mem.tcl |
| Perl Memoized |    0.008 |  | perl fib-mem.pl |
| Python Cached |    0.021 |  | python3 fib-cache.py |
| Ruby Memoized |    0.067 |  | ruby fib-mem.rb |
| Java Iterative |    0.194 | javac FibOptimized.java | java FibOptimized |
| Node Memoized |    0.205 |  | node fib-mem.js |
| Erlang Memoized |    0.271 | erlc +native +'{hipe,[o3]}' fib_mem.erl | erl -noinput -noshell -s fib_mem |
| Escript Memoized |    0.291 |  | escript fib_mem.es |
| Lua Memoized |    0.296 |  | lua fib-mem.lua |
| Perl6 Memoized |    0.357 |  | perl6 fib-mem.p6 |
| Elixir Iterative |    0.688 |  | elixir fib-iterative.exs |

## Versions

All compilers are installed using `apt` or `asdf` on Ubuntu 20.04 docker image:

assembly (gcc) 7.4.0
bash           5.0.0
crystal        1.0.0
clojure        1.10.2.774
cython         0.29.15
D (ldc2)       v1.20.1
dart           2.9.3
dmd            1.20.1
dotnet-core    3.1.403
elixir         1.11.2
elm            0.19.1
erlang         23.1.2
g++            7.4.0
gcc            7.4.0
gfortran       9.3.0
golang         1.14.1
guile          3.0.4
haskell        8.10.1
java           openjdk-16
julia          1.5.0
K              3.6
kotlin         1.3.70
lisp (sbcl)    2.0.3
lua            5.3.0
luaJIT         2.4.4
mono           6.8.0
nim            1.4.2
nodejs         12.16.1
ocaml          4.11.1
pascal         3.0.4
perl           5.30.2
php            7.4.4
ponyc          0.38.1
powershell     v7.0.0
python         3.8.2
R              3.6.3
rakudo         2020.01
ruby           3.0.1
rust           1.42.0
scheme (guile) 2.2
swift          5.3.3
tcl            8.6.10
v              0.2.2
