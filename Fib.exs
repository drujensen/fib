defmodule Fib do
  def fib(n) do 
    if n <= 1 do
      1
    else
      fib(n-1) + fib(n-2) 
    end
  end
end

IO.puts Fib.fib(46)
