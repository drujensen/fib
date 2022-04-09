#!/usr/bin/env escript

-mode(native).

main(_) ->
    io:format("~B~n", [fib(47)]).

fib(0) -> N;
fib(1) -> N;
fib(N) -> fib(N-1) + fib(N-2).
