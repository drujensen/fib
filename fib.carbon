package Fib api;

fn Fib(n: u64) -> u64 {
  if (n <= 1) {
    return n;
  } else {
    return Fib(n - 1) + Fib(n - 2);
  }
}

fn Main() -> u64 {
  return Fib(47);
}
