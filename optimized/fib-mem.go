package main

func fib(n int, cache *[]uint64) uint64 {
	if (*cache)[n] == 0 {
		a := fib(n-2, cache)
		b := fib(n-1, cache)
		(*cache)[n] = a+b
	}
	return (*cache)[n]
}

func main() {
	const n = 46
	cache := make([]uint64, n+1, n+1)
	cache[0] = 1
	cache[1] = 1
	println(fib(n, &cache))
}

