defmodule Fib do
  def fib(n) when n<=1, do: 1
  def fib(n), do: fib(n-1) + fib(n-2)
end

IO.puts Fib.fib(46)
