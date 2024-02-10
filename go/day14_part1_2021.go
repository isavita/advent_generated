package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	polymer := scanner.Text()
	rules := make(map[string]string)

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		parts := strings.Split(line, " -> ")
		rules[parts[0]] = parts[1]
	}

	for step := 0; step < 10; step++ {
		polymer = applyInsertion(polymer, rules)
	}

	counts := countElements(polymer)
	min, max := minMax(counts)

	fmt.Println(max - min)
}

func applyInsertion(polymer string, rules map[string]string) string {
	var newPolymer strings.Builder
	for i := 0; i < len(polymer)-1; i++ {
		newPolymer.WriteString(string(polymer[i]))
		if insert, ok := rules[polymer[i:i+2]]; ok {
			newPolymer.WriteString(insert)
		}
	}
	newPolymer.WriteByte(polymer[len(polymer)-1])
	return newPolymer.String()
}

func countElements(polymer string) map[rune]int {
	counts := make(map[rune]int)
	for _, c := range polymer {
		counts[c]++
	}
	return counts
}

func minMax(counts map[rune]int) (min, max int) {
	min = int(^uint(0) >> 1)
	for _, count := range counts {
		if count < min {
			min = count
		}
		if count > max {
			max = count
		}
	}
	return min, max
}