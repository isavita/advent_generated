package main

import (
	"bufio"
	"fmt"
	"os"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func parseData(data []string) (map[complex128]bool, complex128) {
	garden := make(map[complex128]bool)
	var start complex128 = -1
	for y, line := range data {
		for x, c := range line {
			if c != '#' {
				garden[complex(float64(x), float64(y))] = true
			}
			if c == 'S' {
				start = complex(float64(x), float64(y))
			}
		}
	}

	if start == -1 {
		panic("No start found!")
	}

	return garden, start
}

func complexMod(num complex128, mod int) complex128 {
	if float64(int(real(num))) != real(num) && float64(int(imag(num))) != imag(num) {
		fmt.Println(num, real(num), imag(num))
		panic("Complex number not integer!")
	}
	return complex(float64((int(real(num))+10*mod)%mod), float64((int(imag(num))+10*mod)%mod))
}

func calculateNumEnds(garden map[complex128]bool, start complex128, numIterations int, maxSize int) int {
	queue := make(map[complex128]bool)
	queue[start] = true

	done := make([]int, 0)

	for i := 0; i < 3*maxSize; i++ {
		if (i % maxSize) == (maxSize-1)/2 {
			done = append(done, len(queue))
		}
		if len(done) == 3 {
			break
		}

		newQueue := make(map[complex128]bool)

		for _, dir := range []complex128{1, -1, 1i, -1i} {
			for point := range queue {
				if _, ok := garden[complexMod(point+dir, maxSize)]; ok {
					newQueue[point+dir] = true
				}
			}
		}
		queue = newQueue
	}

	quadraticFunction := func(n, a, b, c int) int {
		return a + n*(b-a+((n-1)*(c-2*b+a)/2))
	}

	return quadraticFunction(numIterations/maxSize, done[0], done[1], done[2])
}

func main() {

	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var sum = 0
	var gardenInput = []string{}

	for scanner.Scan() {
		line := scanner.Text()
		gardenInput = append(gardenInput, line)
	}

	garden, start := parseData(gardenInput)
	maxSize := len(gardenInput)

	sum = calculateNumEnds(garden, start, 26501365, maxSize)

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
	}

	fmt.Println(sum)
}