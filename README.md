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

## How to run them

You can run the tests using Docker: `docker run -it drujensen/fib`

By default, it will compile and run all languages 5 times. Totals are calculated by adding the average compile and run times.

To only run a subset of the languages, provide a list of extensions and optionally the count

`docker run -it drujensen/fib ./run.rb cr,kt,pypy,rbjit 5`

Last benchmark was ran on April 17, 2021

## Natively compiled, statically typed

| Ext | Language | Total | Compile | Time, s | Run | Time, s |
|-----|----------|-------|---------|---------|-----|---------|
| c | C |    5.805 | gcc -fno-inline-small-functions -O3 -o fib fib.c |    0.042 | ./fib |    5.763 |
| f03 | Fortran |    5.818 | gfortran -fno-inline-small-functions -O3 -o fib fib.f03 |    0.058 | ./fib |    5.760 |
| cpp | C++ |    5.823 | g++ -fno-inline-small-functions -O3 -o fib fib.cpp |    0.059 | ./fib |    5.764 |
| d | D |    5.973 | ldc2 -O3 -release -flto=full -of=fib fib.d |    0.373 | ./fib |    5.600 |
| pony | Pony |    6.399 | ponyc -s -b fib -p ./fib.pony |    0.825 | ./fib |    5.574 |
| rs | Rust |    6.535 | rustc -C opt-level=3 fib.rs |    0.363 | ./fib |    6.171 |
| nim | Nim |    6.671 | nim c -d:danger --passC:-fno-inline-small-functions fib.nim |    0.508 | ./fib |    6.163 |
| adb | Ada |    6.963 | gnat make -O2 -gnatp -o fib fib.adb |    0.113 | ./fib |    6.850 |
| pyx | Cython |    7.073 | cython -3 --embed -o fib.pyx.c fib.pyx && gcc -fno-inline-small-functions -O3 -o fib fib.pyx.c $(pkg-config --cflags --libs python) |    0.199 | ./fib |    6.874 |
| swift | Swift |    7.268 | swiftc -O -g fib.swift |    0.182 | ./fib |    7.087 |
| v | V |    7.541 | v -cflags -fno-inline-small-functions -prod -o fib fib.v |    2.015 | ./fib |    5.526 |
| pas | Pascal |    8.065 | fpc -O3 -Si ./fib.pas |    0.052 | ./fib |    8.012 |
| cr | Crystal |    9.093 | crystal build --release fib.cr |    3.242 | ./fib |    5.851 |
| s | Assembly |    9.678 | gcc -no-pie -o fib fib-gcc-x64.s |    0.021 | ./fib |    9.657 |
| ml | OCaml |    9.738 | ocamlopt -O3 -o fib fib.ml |    0.074 | ./fib |    9.664 |
| go | Go |   12.454 | go build fib.go |    0.525 | ./fib |   11.929 |
| hs | Haskell |   12.726 | rm ./fib.o && ghc -O3 -o fib fib.hs |    0.001 | ./fib |   12.725 |
| lisp | Lisp |   16.873 | sbcl --load fib.lisp |    1.771 | ./fib |   15.101 |

## VM compiled bytecode, statically typed

| Ext | Language | Total | Compile | Time, s | Run | Time, s |
|-----|----------|-------|---------|---------|-----|---------|
| java | Java |    8.683 | javac Fib.java |    0.941 | java Fib |    7.741 |
| kt | Kotlin |   11.794 | kotlinc Fib.kt -include-runtime -d Fib.jar |    4.198 | java -jar Fib.jar |    7.596 |
| cs | C# |   14.024 | dotnet build -c Release -o ./bin |    1.949 | dotnet ./bin/fib.dll |   12.075 |
| mono | C# (Mono) |   15.804 | mcs Fib.cs |    0.462 | mono Fib.exe |   15.342 |
| erl | Erlang |   16.107 | erlc +native +'{hipe,[o3]}' fib.erl |    0.509 | erl -noinput -noshell -s fib |   15.598 |

NOTE: DSB Boundary 64 byte alignment may affect results.  See [issue #129](https://github.com/drujensen/fib/issues/129) for details.

## VM compiled before execution, mixed/dynamically typed

| Ext | Language | Time, s | Run |
|-----|----------|---------|-----|
| dart | Dart |   10.824 | dart fib.dart |
| jl | Julia |   11.092 | julia -O3 fib.jl |
| es | Escript |   14.754 | escript fib.es |
| luajit | Lua Jit |   15.925 | luajit fib.lua |
| exs | Elixir |   16.005 | ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]' elixir Fib.exs |
| node | Node |   24.471 | node fib.js |
| cljc | Clojure |   26.522 | clojure -M fib.cljc |
| pypy | Python3 (PyPy) |   44.244 | pypy3 fib.py |
| rbjit | Ruby (jit) |   59.760 | ruby --jit fib.rb |

## Interpreted, dynamically typed

| Ext | Language | Time, s | Run |
|-----|----------|---------|-----|
| php | Php |  112.759 | php fib.php |
| scm | Scheme |  179.639 | guile fib.scm |
| rb | Ruby |  179.711 | ruby fib.rb |
| lua | Lua |  249.706 | lua fib.lua |
| janet | Janet |  281.048 | janet ./fib.janet |
| py | Python |  488.743 | python fib.py |
| py3 | Python3 |  500.493 | python3 fib.py |
| pl | Perl | 1045.041 | perl fib.pl |
| tcl | Tcl | 1526.907 | tclsh fib.tcl |
| p6 | Perl 6 | 2872.754 | perl6 fib.p6 |
| r | R |    DNF | r -f fib.r |
| sh | Bash |    DNF | bash fib.sh |
| ps1 | Powershell |    DNF | pwsh fib.ps1 |

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
