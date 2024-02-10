package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	validTriangles := 0
	for scanner.Scan() {
		sides := strings.Fields(scanner.Text())
		if len(sides) != 3 {
			fmt.Println("Invalid input format")
			continue
		}

		a, _ := strconv.Atoi(sides[0])
		b, _ := strconv.Atoi(sides[1])
		c, _ := strconv.Atoi(sides[2])

		if isValidTriangle(a, b, c) {
			validTriangles++
		}
	}

	fmt.Println(validTriangles)
}

func isValidTriangle(a, b, c int) bool {
	return a+b > c && a+c > b && b+c > a
}