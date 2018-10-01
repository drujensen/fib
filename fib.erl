-module(fib).

-export([start/0]).

start() ->
    io:format("~B~n", [fib(46)]),
    halt().

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).
