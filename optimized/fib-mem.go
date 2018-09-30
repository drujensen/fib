package main

import (
	"fmt"
)

func fib(n uint64, cache []uint64) uint64 {
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
	cache := make([]uint64, 46)
	fmt.Println(fib(46, cache))
}
