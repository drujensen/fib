#!/usr/bin/env perl

use Inline C => <<'...';
 
long fib(long n) {
	         return ( n > 1 ? fib(n - 2) + fib(n - 1) : 1 );
	       } 
...

$L = shift;
      
print fib($L)
      
