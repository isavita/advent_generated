package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))

	var nice int
	passesRule1 := func(line string) bool {
		for i := 0; i < len(line)-2; i++ {
			toMatch := line[i : i+2]
			for j := i + 2; j < len(line)-1; j++ {
				if line[j:j+2] == toMatch {
					return true
				}
			}
		}
		return false
	}

	for _, line := range strings.Split(input, "\n") {
		rule1 := passesRule1(line)

		var rule2 bool
		for i := 0; i < len(line)-2; i++ {
			if line[i] == line[i+2] {
				rule2 = true
				break
			}
		}
		if rule1 && rule2 {
			nice++
		}
	}

	fmt.Println(nice)
}