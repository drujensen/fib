defmodule Fib do
  def  fib(n), do: fib(n, 1, 0)
  defp fib(0, _, result), do: result
  defp fib(n, next, result), do: fib(n-1, next + result, next)
end

IO.puts Fib.fib(46)
