defmodule Fib do
  def fib(0) do 1 end
  def fib(1) do 1 end
  def fib(n) do fib(n-1) + fib(n-2) end
end

IO.puts Fib.fib(46)
