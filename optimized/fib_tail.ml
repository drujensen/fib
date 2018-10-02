(* 
  Tail recursive Fibonacci implementation.
  $ ocamlopt -O3 -o fib-tailrec fib_tailrec.ml
  $ ./fib-tailrec
*)
let fib =
    let rec loop a b = function
        | 1 -> a
        | n -> loop (a + b) a (n - 1)
    in loop 1 1

let () =
  print_int (fib 46);
  print_newline()
