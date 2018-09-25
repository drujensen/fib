package main

import "fmt"

func fib(n int) int {
	if n <= 1 {
		return 1
	}
	return fib(n-2) + fib(n-1)
}

func main() {
	fmt.Println(fib(46))
}
