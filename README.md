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
 - Hetzner CCX23 - 4 vCPU
 - Processor: Intel Xeon 3.1Ghz
 - Memory: 16 GiB
 - OS: Ubuntu 24.04
 - Docker Base Image: ubuntu:24.04


## How to run them

You can run the tests using Docker: `docker run -it drujensen/fib`

By default, it will compile and run all languages 5 times. Totals are calculated by adding the average compile and run times.

To only run a subset of the languages, provide a list of extensions and optionally the count:

`docker run -it drujensen/fib ./run.sh s,c,cpp,go,rs,swift 5`

To run in the background using screen:

```bash
screen
docker run drujensen/fib > results.txt 2>&1
```

- Detach from the session by pressing `Ctrl+A` followed by `D`.
- You can reattach to the session later with:

```bash
screen -r
```

NOTE: Please see [issues](https://github.com/drujensen/fib/issues) with benchmarks like this.  Its original goal was to compare at a marco level how much faster Crystal is to Ruby.  Any language faster than Assembly is performing unrolling type optimizations.  Modern languages like Go, Swift and Crystal have bounds checking which have safety built-in, but also have a cost associated with runtime performance.

# Results

Last benchmark was ran on December 05, 2024

## Natively compiled, statically typed

| Language | Total | Compile | Time | Run | Time | Ext |
|----------|-------|---------|---------|-----|---------|-----|
| C |    4.835 | gcc -O3 -o fib fib.c |    0.112 | ./fib |    4.723 | c |
| C++ |    4.888 | g++ -O3 -o fib fib.cpp |    0.139 | ./fib |    4.750 | cpp |
| Fortran |    6.334 | gfortran -O3 -o fib fib.f03 |    0.130 | ./fib |    6.204 | f03 |
| Ada |    6.811 | gnat make -O3 -gnatp -o fib fib.adb |    0.220 | ./fib |    6.591 | adb |
| Rust |    8.108 | rustc -C opt-level=3 fib.rs |    0.358 | ./fib |    7.750 | rs |
| Odin |    8.838 | odin build fib.odin -file -0:speed |    0.087 | ./fib |    8.751 | odin |
| Mojo |    8.947 | mojo build fib.mojo |    0.241 | ./fib |    8.706 | mojo |
| V |    9.168 | v -prod -o fib fib.v |    4.450 | ./fib |    4.718 | v |
| Assembly |    9.367 | gcc -no-pie -O3 -o fib fib.s |    0.025 | ./fib |    9.342 | s |
| Swift |    9.781 | swiftc -Ounchecked fib.swift |    0.399 | ./fib |    9.381 | swift |
| Pascal |   10.460 | fpc -O3 -Si ./fib.pas |    0.041 | ./fib |   10.419 | pas |
| Pony |   11.106 | ponyc -s -b fib -p ./fib.pony |    0.879 | ./fib |   10.226 | pony |
| OCaml |   16.018 | ocamlopt -O3 -o fib fib.ml |    0.187 | ./fib |   15.831 | ml |
| Zig |   16.181 | zig build-exe -OReleaseFast ./fib.zig |    6.882 | ./fib |    9.299 | zig |
| Haskell |   18.095 | rm ./fib.o && ghc -O3 -o fib fib.hs |    0.001 | ./fib |   18.094 | hs |
| D |   18.193 | dmd -release -of=fib fib.d |    0.342 | ./fib |   17.851 | d |
| Go |   18.948 | go build fib.go |    1.107 | ./fib |   17.842 | go |
| Crystal |   20.525 | crystal build --release fib.cr |    3.106 | ./fib |   17.420 | cr |
| Lisp |   25.726 | sbcl --load fib.lisp |    0.979 | ./fib |   24.747 | lisp |
| Dart Compiled |   31.707 | dart compile exe -o fib ./fib.dart |    1.558 | ./fib |   30.149 | dartc |
| Cobol | 4380.728 | cobc -x -O3 -o fib ./fib.cbl |    0.133 | ./fib | 4380.596 | cbl |

## VM compiled bytecode, statically typed

| Language | Total | Compile | Time | Run | Time | Ext |
|----------|-------|---------|---------|-----|---------|-----|
| C# |   12.760 | dotnet build -c Release -o ./bin |    2.015 | dotnet ./bin/fib.dll |   10.744 | cs |
| Java |   13.040 | javac Fib.java |    0.733 | java Fib |   12.307 | java |
| Scala |   15.836 | scalac Fib.scala |    2.682 | scala Fib |   13.153 | scala |
| Kotlin |   16.286 | kotlinc Fib.kt |    4.074 | java FibKt |   12.212 | kt |
| Erlang |   28.378 | erlc +native +'{hipe,[o3]}' fib.erl |    0.402 | erl -noinput -noshell -s fib |   27.976 | erl |
| Groovy |   70.123 | groovyc Fib.groovy |    1.519 | groovy Fib |   68.604 | groovy |

## VM compiled before execution, mixed/dynamically typed

| Language | Time | Run | Ext |
|----------|---------|-----|-----|
| Clojure |   17.815 | clojure -M fib.cljc | cljc |
| Julia |   18.000 | julia -O3 fib.jl | jl |
| Bun |   21.312 | bun fib.js | bun |
| Dart |   29.984 | dart fib.dart | dart |
| Node |   34.736 | node fib.js | js |
| Elixir |   35.243 | ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]' elixir Fib.exs | exs |
| Lua Jit |   37.837 | luajit fib.lua | luajit |
| Python (PyPy) |   54.078 | pypy fib.py | pypy |
| Ruby (jit) |   81.454 | ruby --jit fib.rb | rbjit |

## Interpreted, dynamically typed

| Language | Time | Run | Ext |
|----------|---------|-----|-----|
| Escript |   28.380 | escript fib.es | es |
| Scheme |  102.887 | guile fib.scm | scm |
| Php |  157.312 | php fib.php | php |
| Lua |  203.702 | lua fib.lua | lua |
| Ruby |  393.625 | ruby fib.rb | rb |
| Python | 423.427 | python fib.py | py |
| Janet |  479.663 | janet ./fib.janet | janet |
| Perl | 1490.416 | perl fib.pl | pl |
| Raku | 1672.015 | rakudo fib.raku | raku |
| Tcl | 2230.883 | tclsh fib.tcl | tcl |
| R | 2575.249 | R -f fib.r | r |

## Versions
All compilers are installed using apt or asdf on Ubuntu 24.04 docker image:

|Language | Version |
|---|---|
| ada | 13.2.0 |
| assembly | 13.2.0 |
| bash | 5.2.21 |
| bun | 1.1.38 |
| crystal | 1.14.0 |
| clojure | 1.12.0.1488 |
| dart | 3.5.4 |
| dmd | 2.109.1 |
| dotnet | 9.0.101 |
| elixir | 1.17.3 |
| elm | 0.19.1 |
| erlang | 27.1.2 |
| fortran | 13.2.0 |
| g++ | 13.2.0 |
| gcc | 13.2.0 |
| gnucobol | 3.2.0 |
| golang | 1.23.2 |
| groovy | 4.0.24 |
| guile | 3.0.10 |
| haskell | 9.8.3 |
| janet | 1.36.0 |
| java | openjdk-23 |
| julia | 1.11.1 |
| K | 3.6 |
| kotlin | 2.1.0 |
| ldc | 1.39.0 |
| lua | 5.4.7 |
| luajit | 2.1.1 |
| mojo | 24.5.0 |
| nim | 2.2.0 |
| nodejs | 23.3.0 |
| ocaml | 5.2.1 |
| odin | dev-2024-11 |
| pascal | 3.2.2 |
| perl | 5.40.0 |
| php | 8.4.1 |
| pony | 0.58.7 |
| powershell-core | 7.4.6 |
| python | 3.12.0 |
| pypy | 7.3.17 |
| r | 4.4.2 |
| rakudo | 2024.10 |
| ruby | 3.3.6 |
| rust | 1.83.0 |
| sbcl | 2.4.11 |
| scala | 3.3.4 |
| swift | 6.0.2 |
| tcl | 9.0.0 |
| v | 0.4.8 |
| zig | 0.13.0 |
