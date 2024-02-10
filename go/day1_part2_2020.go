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
		panic(err)
	}
	defer file.Close()

	var expenses []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		num, _ := strconv.Atoi(scanner.Text())
		expenses = append(expenses, num)
	}

	for i := 0; i < len(expenses); i++ {
		for j := i + 1; j < len(expenses); j++ {
			for k := j + 1; k < len(expenses); k++ {
				if expenses[i]+expenses[j]+expenses[k] == 2020 {
					fmt.Println(expenses[i] * expenses[j] * expenses[k])
					return
				}
			}
		}
	}
}