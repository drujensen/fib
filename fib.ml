let rec fib i =
  if i <= 1 then 1 else fib (i - 1) + fib (i - 2)

let () =
  print_int (fib 46);
  print_newline ()
