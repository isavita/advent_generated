package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var replacements []string
	var molecule string
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		if strings.Contains(line, " => ") {
			replacements = append(replacements, line)
		} else {
			molecule = line
		}
	}

	molecules := make(map[string]bool)
	for _, replacement := range replacements {
		parts := strings.Split(replacement, " => ")
		for i := 0; i < len(molecule); i++ {
			if strings.HasPrefix(molecule[i:], parts[0]) {
				newMolecule := molecule[:i] + parts[1] + molecule[i+len(parts[0]):]
				molecules[newMolecule] = true
			}
		}
	}

	fmt.Println(len(molecules))
}
