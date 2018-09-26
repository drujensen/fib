fn fib(n: u64) -> u64 {
  if n <= 1 { return 1 }
  fib(n - 1) + fib(n - 2)
}

fn main() {
  println!("{}", fib(46));
}
