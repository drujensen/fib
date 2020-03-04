program Fib;

{$mode ObjFPC}

function Fib(const N: UInt64): UInt64; inline;
begin
  case N of
    0, 1: Result := 1;
    otherwise Result := Fib(N - 1) + Fib(N - 2);
  end;
end;

begin
  WriteLn(Fib(46));
end.
