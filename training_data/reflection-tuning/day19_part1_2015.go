package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	replacements := make(map[string][]string)
	var molecule string

	// Parse input
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		if !strings.Contains(line, "=>") {
			molecule = line
			break
		}
		parts := strings.Split(line, " => ")
		replacements[parts[0]] = append(replacements[parts[0]], parts[1])
	}

	// Generate new molecules
	uniqueMolecules := make(map[string]bool)
	for from, tos := range replacements {
		indices := findAllIndices(molecule, from)
		for _, index := range indices {
			for _, to := range tos {
				newMolecule := molecule[:index] + to + molecule[index+len(from):]
				uniqueMolecules[newMolecule] = true
			}
		}
	}

	fmt.Println(len(uniqueMolecules))
}

func findAllIndices(s, substr string) []int {
	var indices []int
	index := 0
	for {
		i := strings.Index(s[index:], substr)
		if i == -1 {
			break
		}
		indices = append(indices, index+i)
		index += i + 1
	}
	return indices
}
