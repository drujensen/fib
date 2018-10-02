#!/usr/bin/env escript

-mode(native).

main(_) ->
    io:format("~B~n", [fib(46)]).

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).
