package main

import "fmt"

func fib(n uint64, cache map[uint64]uint64) uint64 {
	if n > 1 {
		a, ok := cache[n-1]
		if !ok {
			a = fib(n-1, cache)
			cache[n-1] = a
		}
		b, ok := cache[n-2]
		if !ok {
			b = fib(n-2, cache)
			cache[n-2] = b
		}
		return a + b
	}
	return 1
}

func main() {
	cache := make(map[uint64]uint64)
	fmt.Println(fib(uint64(46), cache))
}
