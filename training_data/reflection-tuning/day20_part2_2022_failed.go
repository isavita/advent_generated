package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type Number struct {
	value    int64
	position int
}

func main() {
	numbers := readInput("input.txt")
	fmt.Println("Part 1:", solve(numbers, 1, 1))
	fmt.Println("Part 2:", solve(numbers, 811589153, 10))
}

func readInput(filename string) []int64 {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	var numbers []int64
	for scanner.Scan() {
		num, _ := strconv.ParseInt(scanner.Text(), 10, 64)
		numbers = append(numbers, num)
	}
	return numbers
}

func solve(numbers []int64, key int64, mixCount int) int64 {
	n := len(numbers)
	mixed := make([]Number, n)
	for i, num := range numbers {
		mixed[i] = Number{value: num * key, position: i}
	}

	for c := 0; c < mixCount; c++ {
		for i := 0; i < n; i++ {
			for j := 0; j < n; j++ {
				if mixed[j].position == i {
					newPos := (j + int(mixed[j].value)) % (n - 1)
					if newPos <= 0 {
						newPos += n - 1
					}
					if newPos > j {
						for k := j; k < newPos; k++ {
							mixed[k] = mixed[k+1]
							mixed[k].position--
						}
					} else {
						for k := j; k > newPos; k-- {
							mixed[k] = mixed[k-1]
							mixed[k].position++
						}
					}
					mixed[newPos] = Number{value: mixed[j].value, position: i}
					break
				}
			}
		}
	}

	zeroIndex := 0
	for i, num := range mixed {
		if num.value == 0 {
			zeroIndex = i
			break
		}
	}

	result := int64(0)
	for _, offset := range []int{1000, 2000, 3000} {
		result += mixed[(zeroIndex+offset)%n].value
	}
	return result
}
