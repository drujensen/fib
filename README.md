# Recursive Fibonacci Benchmark using top languages on Github

Top 10: JavaScript, Python, Java, TypeScript, C#, Php, C++, C, Shell, Ruby [reference](https://octoverse.github.com)

Others: Go, Rust, Swift, Crystal, Pony, Ada, Pascal, Fortran, Kotlin, Clojure, Scala, Mono, R, Dart, Julia, D, Nim, Cython, Python3, PyPy, Ruby jit, OCaml, Lisp, Haskell, Erlang, Elixir, Escript, Dart, Scheme, Lua, Perl, Perl6, Bash, Emoji

The code performs a recursive fibonacci to the 47th position with the result of 2,971,215,073.

Fibonacci can be written many different ways.  The goal of this project is to compare how each language handles the exact same code.

Here is the Ruby version:
```
def fib(n)
  return n if n <= 1
  fib(n - 1) + fib(n - 2)
end

puts fib(47)
```

Here is the Crystal version:
```
def fib(n)
  return n if n <= 1
  fib(n - 1) + fib(n - 2)
end

puts fib(47_u64)
```

Too keep a level playing field, only common "release" flags are used in the compilation step.  This allows for compiler optimizations like inlining and constant propogation but removes anything considered dangerous i.e. bypassing out of bounds checks.

All tests are run on:
 - AWS EC2 - m5.large 2 vCPU
 - Processor: Intel Xeon 3.1Ghz
 - Memory: 8 GiB
 - OS: Ubuntu 20.04
 - Docker Base Image: ubuntu:20.04


## How to run them

You can run the tests using Docker: `docker run -it drujensen/fib`

By default, it will compile and run all languages 5 times. Totals are calculated by adding the average compile and run times.

To only run a subset of the languages, provide a list of extensions and optionally the count:

`docker run -it drujensen/fib ./run.sh s,c,cpp,go,rs,swift 5`

To run in the background using nohup:

`nohup docker run drujensen/fib:latest > results.txt 2>&1 &`

# Results

Last benchmark was ran on January 18, 2022

## Natively compiled, statically typed

| Language | Total | Compile | Time, s | Run | Time, s | Ext |
|----------|-------|---------|---------|-----|---------|-----|
| C |    6.865 | gcc -O3 -o fib fib.c |    0.058 | ./fib |    6.807 | c |
| Fortran |    6.893 | gfortran -O3 -o fib fib.f03 |    0.119 | ./fib |    6.774 | f03 |
| C++ |    6.954 | g++ -O3 -o fib fib.cpp |    0.126 | ./fib |    6.828 | cpp |
| Cython |    7.114 | cython -3 --embed -o fib.pyx.c fib.pyx && gcc -O3 -o fib fib.pyx.c $(pkg-config --cflags --libs python) |    0.297 | ./fib |    6.817 | pyx |
| Nim |    7.337 | nim c -d:release fib.nim |    0.920 | ./fib |    6.416 | nim |
| Assembly |    7.401 | gcc -no-pie -O3 -o fib fib.s |    0.025 | ./fib |    7.376 | s |
| Ada |    7.645 | gnat make -O3 -gnatp -o fib fib.adb |    0.213 | ./fib |    7.433 | adb |
| Rust |    7.652 | rustc -C opt-level=3 fib.rs |    0.638 | ./fib |    7.014 | rs |
| Pascal |    8.845 | fpc -O3 -Si ./fib.pas |    0.078 | ./fib |    8.766 | pas |
| Pony |    9.117 | ponyc -s -b fib -p ./fib.pony |    1.038 | ./fib |    8.079 | pony |
| D |    9.481 | ldc2 -O3 -release -flto=full -of=fib fib.d |    0.612 | ./fib |    8.869 | d |
| Swift |   10.568 | swiftc -O -g fib.swift |    0.474 | ./fib |   10.094 | swift |
| OCaml |   10.729 | ocamlopt -O3 -o fib fib.ml |    0.294 | ./fib |   10.435 | ml |
| V |   10.752 | v -prod -o fib fib.v |    4.352 | ./fib |    6.399 | v |
| Go |   12.486 | go build fib.go |    0.600 | ./fib |   11.885 | go |
| Haskell |   12.544 | rm ./fib.o && ghc -O3 -o fib fib.hs |    0.001 | ./fib |   12.543 | hs |
| Crystal |   17.900 | crystal build --release fib.cr |    4.940 | ./fib |   12.960 | cr |
| Lisp |   17.996 | sbcl --load fib.lisp |    1.704 | ./fib |   16.293 | lisp |
| Dart Compiled |   30.942 | dart compile exe -o fib ./fib.dart |    5.058 | ./fib |   25.884 | dartc |
| Cobol | 4380.728 | cobc -x -O3 -o fib ./fib.cbl |    0.133 | ./fib | 4380.596 | cbl |

## VM compiled bytecode, statically typed

| Language | Total | Compile | Time, s | Run | Time, s | Ext |
|----------|-------|---------|---------|-----|---------|-----|
| Scala |    9.047 | scalac Fib.scala |    0.942 | scala Fib |    8.104 | scala |
| Java |   10.391 | javac Fib.java |    0.894 | java Fib |    9.497 | java |
| Kotlin |   11.721 | kotlinc Fib.kt |    2.097 | java FibKt |    9.624 | kt |
| C# (Mono) |   16.107 | mcs Fib.cs |    0.491 | mono Fib.exe |   15.616 | mono |
| C# |   16.218 | dotnet build -c Release -o ./bin |    2.162 | dotnet ./bin/fib.dll |   14.056 | cs |
| Erlang |   28.868 | erlc +native +'{hipe,[o3]}' fib.erl |    0.500 | erl -noinput -noshell -s fib |   28.368 | erl |

## VM compiled before execution, mixed/dynamically typed

| Language | Time, s | Run | Ext |
|----------|---------|-----|-----|
| Dart |   11.286 | dart fib.dart | dart |
| Julia |   11.318 | julia -O3 fib.jl | jl |
| Lua Jit |   16.382 | luajit fib.lua | luajit |
| Clojure |   23.448 | clojure -M fib.cljc | cljc |
| Elixir |   28.604 | ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]' elixir Fib.exs | exs |
| Node |   30.232 | node fib.js | js |
| Python3 (PyPy) |   37.827 | pypy3 fib.py | pypy |
| Ruby (jit) |   51.793 | ruby --jit fib.rb | rbjit |

## Interpreted, dynamically typed

| Language | Time, s | Run | Ext |
|----------|---------|-----|-----|
| Scheme |   66.668 | guile fib.scm | scm |
| Php |  118.468 | php fib.php | php |
| Lua |  169.710 | lua fib.lua | lua |
| Ruby |  223.653 | ruby fib.rb | rb |
| Janet |  292.366 | janet ./fib.janet | janet |
| Python |  507.845 | python fib.py | py |
| Python3 |  527.247 | python3 fib.py | py3 |
| Perl | 1085.181 | perl fib.pl | pl |
| Tcl | 1580.804 | tclsh fib.tcl | tcl |
| R | 2413.682 | R -f fib.r | r |
| Raku | 2841.391 | rakudo fib.raku | raku |
| Escript | 3448.282 | escript fib.es | es |

## Versions

All compilers are installed using apt or asdf on Ubuntu 20.04 docker image.

| language | version |
|----------|---------|
| ada | 9.3.0 |
| assembly | 9.3.0 |
| bash | 5.0.0 |
| crystal | 1.3.0 |
| clojure | 1.10.3.1040 |
| cython | 0.29.26 |
| dart | 2.15.1 |
| dotnet-core | 6.0.101 |
| elixir | 1.12.0 |
| elm | 0.19.1 |
| erlang | 24.2 |
| fortran | 9.3.0 |
| g++ | 9.3.0 |
| gcc | 9.3.0 |
| golang | 1.17.5 |
| guile | 3.0.7 |
| haskell | 9.2.1 |
| janet | 1.19.2 |
| java | openjdk-17 |
| julia | 1.7.1 |
| K | 3.6 |
| kotlin | 1.6.10 |
| ldc2 | 1.2.4 |
| lua | 5.4.3 |
| luaJIT | 2.1.0 |
| mono | 6.8.0 |
| nim | 1.6.2 |
| nodejs | 17.3.0 |
| ocaml | 4.11.1 |
| pascal | 3.0.4 |
| perl | 5.34.0 |
| php | 8.1.1 |
| pony | 0.38.1 |
| powershell | v7.0.0 |
| python | 3.10.1 |
| pypy | 7.3.7 |
| qb64 | 1.5 |
| R | 4.1.2 |
| rakudo | 2021.12 |
| ruby | 3.1.0 |
| rust | 1.57.0 |
| sbcl | 2.11.1 |
| scala | 3.1.0 |
| swift | 5.5.2 |
| tcl | 8.6.10 |
| v | 0.2.2 |
