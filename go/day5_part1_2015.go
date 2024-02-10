package main

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))

	var nice int
	disallowPattern := regexp.MustCompile("(ab|cd|pq|xy)")
	for _, line := range strings.Split(input, "\n") {
		var vowels int
		for _, char := range line {
			if strings.ContainsRune("aeiou", char) {
				vowels++
			}
		}
		var hasDouble bool
		for i := 0; i < len(line)-1; i++ {
			if line[i] == line[i+1] {
				hasDouble = true
				break
			}
		}
		if vowels >= 3 && !disallowPattern.MatchString(line) && hasDouble {
			nice++
		}
	}

	fmt.Println(nice)
}