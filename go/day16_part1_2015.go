package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	mfcsam := map[string]int{
		"children": 3, "cats": 7, "samoyeds": 2, "pomeranians": 3,
		"akitas": 0, "vizslas": 0, "goldfish": 5, "trees": 3,
		"cars": 2, "perfumes": 1,
	}

	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " ")
		sueNumber := parts[1][:len(parts[1])-1]

		matches := true
		for i := 2; i < len(parts); i += 2 {
			item := parts[i][:len(parts[i])-1]
			count, _ := strconv.Atoi(parts[i+1][:len(parts[i+1])-1])
			if mfcsam[item] != count {
				matches = false
				break
			}
		}

		if matches {
			fmt.Println(sueNumber)
			break
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
	}
}