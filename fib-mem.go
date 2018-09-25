package main

import (
	"fmt"
)

func fib(n uint32, cache []uint32) uint32 {
	if n <= 1 {
		return 1
	}
	a, b := &cache[n-1], &cache[n-2]
	if *a == 0 {
		*a = fib(n-1, cache)
	}
	if *b == 0 {
		*b = fib(n-2, cache)
	}
	return *a + *b
}

func main() {
	cache := make([]uint32, 46)
	fmt.Println(fib(46, cache))
}
