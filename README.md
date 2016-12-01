# Recursive Fibonacci using top languages on github
js, java, python, ruby, php, c++, c#, c, go

[reference](http://www.techworm.net/2016/09/top-10-popular-programming-languages-github.html)

Including crystal-lang for comparison

## Natively compiled, statically typed

### C - 6.999 Seconds
```bash
gcc -o fib fib.c
time ./fib
```

### C++ - 7.166 Seconds
```bash
g++ -o fib fib.cpp
time ./fib
```

### Go - 6.703 Seconds
```bash
go build fib.go
time ./fib
```

### Crystal - 3.857 Seconds
```bash
crystal build fib.cr --release
time ./fib
```

## VM bytecode, statically typed

### C# using DotNet - 
```bash
dotnet restore
time dotnet run
```

### C# using Mono - 7.166 Seconds
```bash
mcs fib.cs
time mono fib.exe
```

### Java - 4.672 Seconds
```bash
javac Fib.java
time java Fib
```

## Interpreted, dynamically typed

### Node - 12.76 Seconds
```bash
time node fib.js
```

### Ruby - 2 Minutes 4 Seconds
```bash
time ruby fib.rb
```

### Python - 5 Minutes 44 seconds
```bash
time python fib.py
```

### PHP - 6 Minutes 2 Seconds
```bash
time php fib.php
```

