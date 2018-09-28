defmodule Fib do
  def fib(0), do: 1
  def fib(1), do: 1
  def fib(n), do: fib(n-1) + fib(n-2)
end

IO.puts Fib.fib(46)
