let rec fib n =
  if n <= 1 then n else fib (n - 1) + fib (n - 2)

let () =
  print_int (fib 47);
  print_newline ()
