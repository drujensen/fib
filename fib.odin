package main

import "core:fmt"

fib :: proc( n : u64 ) -> u64 {
	if n <= 1 {
		return n
	}

	return fib( n - 1 ) + fib( n - 2 )
}

main :: proc() {
	r := fib(47)

	fmt.println( r )
}
