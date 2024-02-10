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

	var numbers [][]int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		sides := strings.Fields(scanner.Text())
		var row []int
		for _, side := range sides {
			num, _ := strconv.Atoi(side)
			row = append(row, num)
		}
		numbers = append(numbers, row)
	}

	validTriangles := 0
	for i := 0; i < len(numbers[0]); i++ {
		for j := 0; j < len(numbers); j += 3 {
			if j+2 < len(numbers) && isValidTriangle(numbers[j][i], numbers[j+1][i], numbers[j+2][i]) {
				validTriangles++
			}
		}
	}

	fmt.Println(validTriangles)
}

func isValidTriangle(a, b, c int) bool {
	return a+b > c && a+c > b && b+c > a
}