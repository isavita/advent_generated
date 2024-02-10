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
		fmt.Println(err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var horizontalPosition int
	var depth int

	for scanner.Scan() {
		command := strings.Split(scanner.Text(), " ")
		direction := command[0]
		units, _ := strconv.Atoi(command[1])

		switch direction {
		case "forward":
			horizontalPosition += units
		case "down":
			depth += units
		case "up":
			depth -= units
		}
	}

	product := horizontalPosition * depth
	fmt.Println(product)
}