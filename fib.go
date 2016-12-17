package main

import "fmt"

func fib(n uint64) uint64 {
	if n <= 1 {
		return 1
	} else {
		return fib(n - 1) + fib(n - 2)
	}
}

func main() {
	fmt.Println(fib(uint64(46)))
}
