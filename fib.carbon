package Fib api;

fn Fib(n: i32) -> i32 {
  if (n <= 1) {
    return n;
  } else {
    return Fib(n - 1) + Fib(n - 2);
  }
}

fn Main() -> i32 {
  return Fib(45);
}
