package main

import (
	"fmt"
)

type fibCache []uint64

func (c *fibCache) fib(n int) uint64 {
	if n >= len(*c) {
		if n > cap(*c) {
			if *c == nil {
				*c = make(fibCache, 2, n)
				(*c)[0], (*c)[1] = 1, 1
			} else {
				old := *c
				*c = make(fibCache, len(old), n)
				copy(*c, old)
			}
		}
		a := c.fib(n - 2)
		b := c.fib(n - 1)
		*c = append(*c, a+b)
	}
	return (*c)[n]
}

func main() {
	fmt.Println(new(fibCache).fib(46))
}
