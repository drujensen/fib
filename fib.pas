program Fib;
function fib(n: int64): int64;
begin
	if n > 2 then
		fib := (fib(n-1) + fib(n-2))
	else
		fib := 1;
end;
begin
    writeln(fib(46))
end.
