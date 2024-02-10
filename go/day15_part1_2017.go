package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	genAStart, _ := strconv.ParseInt(scanner.Text(), 10, 64)

	scanner.Scan()
	genBStart, _ := strconv.ParseInt(scanner.Text(), 10, 64)

	genAFactor := int64(16807)
	genBFactor := int64(48271)
	modulus := int64(2147483647)

	genA := genAStart
	genB := genBStart
	matches := 0

	for i := 0; i < 40000000; i++ {
		genA = (genA * genAFactor) % modulus
		genB = (genB * genBFactor) % modulus

		if genA&0xFFFF == genB&0xFFFF {
			matches++
		}
	}

	fmt.Println(matches)
}