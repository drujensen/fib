-module(fib).

-export([start/0]).

start() ->
    io:format("~B~n", [fib(46)]),
    halt().

fib(N) when N=<1 -> 1;
fib(N) -> fib(N-1) + fib(N-2).
