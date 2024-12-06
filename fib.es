#!/usr/bin/env escript

-mode(native).

main(_) ->
    io:format("~B~n", [fib(47)]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).
