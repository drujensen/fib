actor Main
  fun fib(n: U64): U64 =>
    if n <= 1 then
      n 
    else
      fib(n -~ 1) + fib(n -~ 2)
    end

  new create(env: Env) =>
    env.out.print(fib(47).string())
