package main

import (
	"fmt"
)

func isPrime(n int) bool {
	for i := 2; i*i <= n; i++ {
		if n%i == 0 {
			return false
		}
	}
	return true
}

func main() {
	b := 57*100 + 100000 // Initial value computed for register b
	c := b + 17000       // Initial value computed for register c
	h := 0               // Initialize register h

	for x := b; x <= c; x += 17 { // Simulate the loop from b to c with step 17
		if !isPrime(x) {
			h++
		}
	}

	fmt.Println(h)
}