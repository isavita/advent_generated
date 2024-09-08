package main

import (
	"fmt"
)

type Elf struct {
	position int
	next     *Elf
	prev     *Elf
}

func solvePartTwo(n int) int {
	// Create the circle
	first := &Elf{position: 1}
	current := first
	for i := 2; i <= n; i++ {
		newElf := &Elf{position: i}
		current.next = newElf
		newElf.prev = current
		current = newElf
	}
	current.next = first
	first.prev = current

	// Simulate the gift stealing
	current = first
	remaining := n
	for remaining > 1 {
		// Find the Elf across the circle
		across := current
		for i := 0; i < remaining/2; i++ {
			across = across.next
		}

		// Remove the Elf across the circle
		across.prev.next = across.next
		across.next.prev = across.prev

		remaining--
		current = current.next
	}

	return current.position
}

func main() {
	input := 3018458 // Your puzzle input
	result := solvePartTwo(input)
	fmt.Println(result)
}
