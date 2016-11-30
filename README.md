# Recursive Fibonacci using top languages on github
js, java, python, ruby, php, c++, c#, c, go

[reference](http://www.techworm.net/2016/09/top-10-popular-programming-languages-github.html)

Including crystal-lang for comparison

## Natively compiled

### C
gcc -o fib fib.c
time ./fib

### C++
g++ -o fib fib.cpp
time ./fib

### Go
go build fib.go
time ./fib

### Crystal
crystal build fib.cr --release
time ./fib

## VM languages

### C# using DotNet
dotnet restore
time dotnet run

### C# using Mono
mcs fib.cs
time mono fib.exe

### Java
javac Fib.java
time java Fib

## Interpreted languages

### Node
time node fib.js

### Ruby
time ruby fib.rb

### Python
time python fib.py

### PHP
time php fib.php
