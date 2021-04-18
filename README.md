# Recursive Fibonacci Benchmark using top languages on Github

Top 10: C, Java, Python, C++, C#, JavaScript, Assembly, Php, Ruby, Go [reference](https://www.tiobe.com/tiobe-index/)

Others: Rust, Swift, Crystal, Pony, Ada, Pascal, Fortran, Kotlin, Clojure, Scala, Mono, R, Dart, Julia, D, Nim, Cython, Python3, PyPy, Ruby jit, OCaml, Lisp, Haskell, Erlang, Elixir, Escript, Dart, Scheme, Lua, Perl, Perl6, Bash, Emoji

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

## How to run them

You can run the tests using Docker: `docker run -it drujensen/fib`

By default, it will compile and run all languages 5 times. Totals are calculated by adding the average compile and run times.

To only run a subset of the languages, provide a list of extensions and optionally the count

`docker run -it drujensen/fib ./run.sh cr,kt,pypy,rbjit 5`

Last benchmark was ran on April 18, 2021

## Natively compiled, statically typed

| Language | Total | Compile | Time, s | Run | Time, s | Ext |
|----------|-------|---------|---------|-----|---------|-----|
| C |    5.842 | gcc -fno-inline-small-functions -O3 -o fib fib.c |    0.078 | ./fib |    5.765 | c |
| Fortran |    5.866 | gfortran -fno-inline-small-functions -O3 -o fib fib.f03 |    0.104 | ./fib |    5.762 | f03 |
| C++ |    5.872 | g++ -fno-inline-small-functions -O3 -o fib fib.cpp |    0.106 | ./fib |    5.766 | cpp |
| D |    6.047 | ldc2 -O3 -release -flto=full -of=fib fib.d |    0.442 | ./fib |    5.604 | d |
| Assembly |    6.151 | gcc -no-pie -o fib fib.s |    0.022 | ./fib |    6.129 | s |
| Pony |    6.484 | ponyc -s -b fib -p ./fib.pony |    0.907 | ./fib |    5.577 | pony |
| Rust |    6.555 | rustc -C opt-level=3 fib.rs |    0.380 | ./fib |    6.175 | rs |
| Nim |    6.696 | nim c -d:danger --passC:-fno-inline-small-functions fib.nim |    0.531 | ./fib |    6.165 | nim |
| Ada |    7.047 | gnat make -O2 -gnatp -o fib fib.adb |    0.195 | ./fib |    6.852 | adb |
| Cython |    7.116 | cython -3 --embed -o fib.pyx.c fib.pyx && gcc -fno-inline-small-functions -O3 -o fib fib.pyx.c $(pkg-config --cflags --libs python) |    0.240 | ./fib |    6.877 | pyx |
| Swift |    7.283 | swiftc -O -g fib.swift |    0.194 | ./fib |    7.089 | swift |
| V |    7.605 | v -cflags -fno-inline-small-functions -prod -o fib fib.v |    2.076 | ./fib |    5.528 | v |
| Pascal |    8.084 | fpc -O3 -Si ./fib.pas |    0.066 | ./fib |    8.018 | pas |
| Crystal |    9.226 | crystal build --release fib.cr |    3.374 | ./fib |    5.852 | cr |
| OCaml |    9.746 | ocamlopt -O3 -o fib fib.ml |    0.079 | ./fib |    9.667 | ml |
| Go |   12.462 | go build fib.go |    0.527 | ./fib |   11.935 | go |
| Haskell |   12.732 | rm ./fib.o && ghc -O3 -o fib fib.hs |    0.002 | ./fib |   12.730 | hs |
| Lisp |   16.923 | sbcl --load fib.lisp |    1.818 | ./fib |   15.106 | lisp |

NOTE: DSB Boundary 64 byte alignment may affect results.  See [issue #129](https://github.com/drujensen/fib/issues/129) for details.

## VM compiled bytecode, statically typed

| Language | Total | Compile | Time, s | Run | Time, s | Ext |
|----------|-------|---------|---------|-----|---------|-----|
| Java |    9.880 | javac Fib.java |    0.907 | java Fib |    8.972 | java |
| Scala |   10.060 | scalac Fib.scala |    1.661 | scala Fib |    8.399 | scala |
| Kotlin |   12.127 | kotlinc Fib.kt -include-runtime -d Fib.jar |    4.357 | java -jar Fib.jar |    7.771 | kt |
| C# |   14.350 | dotnet build -c Release -o ./bin |    2.018 | dotnet ./bin/fib.dll |   12.333 | cs |
| C# (Mono) |   15.860 | mcs Fib.cs |    0.438 | mono Fib.exe |   15.422 | mono |
| Erlang |   16.167 | erlc +native +'{hipe,[o3]}' fib.erl |    0.537 | erl -noinput -noshell -s fib |   15.630 | erl |

## VM compiled before execution, mixed/dynamically typed

| Language | Time, s | Run | Ext |
|----------|---------|-----|-----|
| Dart |   10.819 | dart fib.dart | dart |
| Julia |   10.949 | julia -O3 fib.jl | jl |
| Escript |   14.810 | escript fib.es | es |
| Lua Jit |   15.896 | luajit fib.lua | luajit |
| Elixir |   16.242 | ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]' elixir Fib.exs | exs |
| Node |   24.434 | node fib.js | node |
| Clojure |   26.591 | clojure -M fib.cljc | cljc |
| Python3 (PyPy) |   52.748 | pypy3 fib.py | pypy |
| Ruby (jit) |   60.363 | ruby --jit fib.rb | rbjit |

## Interpreted, dynamically typed

| Language | Time, s | Run | Ext |
|----------|---------|-----|-----|
| Php |  112.758 | php fib.php | php |
| Scheme |  179.906 | guile fib.scm | scm |
| Ruby |  179.997 | ruby fib.rb | rb |
| Lua |  249.807 | lua fib.lua | lua |
| Janet |  280.298 | janet ./fib.janet | janet |
| Python |  485.371 | python fib.py | py |
| Python3 |  500.889 | python3 fib.py | py3 |
| Perl | 1043.080 | perl fib.pl | pl |
| Tcl | 1528.249 | tclsh fib.tcl | tcl |
| R | 2025.003 | R -f fib.r | r |
| Perl 6 | 2957.837 | perl6 fib.p6 | p6 |
| Bash |    DNF | bash fib.sh | sh |
| Powershell |    DNF | pwsh fib.ps1 | ps1 |

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
scala          2.13.5
scheme (guile) 2.2
swift          5.3.3
tcl            8.6.10
v              0.2.2
