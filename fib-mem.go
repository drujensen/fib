package main

import (
	"fmt"
)

func fib(n int, cache *[]uint64) uint64 {
	if n >= len(*cache) {
		a := fib(n-2, cache)
		b := fib(n-1, cache)
		*cache = append(*cache, a+b)
	}
	return (*cache)[n]
}

func main() {
	const n = 46
	cache := make([]uint64, 2, n)
	cache[0] = 1
	cache[1] = 1
	fmt.Println(fib(n, &cache))
}
