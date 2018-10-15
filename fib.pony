actor Main
  fun fib(n: U64): U64 =>
    if n <= 1 then
      1
    else
      fib(n - 1) + fib(n - 2)
    end

  new create(env: Env) =>
    env.out.print(fib(46).string())
