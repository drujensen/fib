num fib(num n) {
  if (n <= 1) return 1;
  return fib(n - 1) + fib(n - 2);
}

main () {
  print(fib(46));
}
