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

	for i := 0; i < 5000000; i++ {
		// Generate next value for A that is a multiple of 4
		for {
			genA = (genA * genAFactor) % modulus
			if genA%4 == 0 {
				break
			}
		}

		// Generate next value for B that is a multiple of 8
		for {
			genB = (genB * genBFactor) % modulus
			if genB%8 == 0 {
				break
			}
		}

		if genA&0xFFFF == genB&0xFFFF {
			matches++
		}
	}

	fmt.Println(matches)
}