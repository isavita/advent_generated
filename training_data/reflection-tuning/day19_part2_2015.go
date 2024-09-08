package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"strings"
)

type Replacement struct {
	from, to string
}

func main() {
	replacements, medicine := parseInput("input.txt")
	
	// Part One
	molecules := generateMolecules(replacements, medicine)
	fmt.Println("Part One:", len(molecules))
	
	// Part Two
	steps := findMinSteps(replacements, medicine)
	fmt.Println("Part Two:", steps)
}

func parseInput(filename string) ([]Replacement, string) {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	
	var replacements []Replacement
	var medicine string
	
	for scanner.Scan() {
		line := scanner.Text()
		if strings.Contains(line, "=>") {
			parts := strings.Split(line, " => ")
			replacements = append(replacements, Replacement{parts[0], parts[1]})
		} else if line != "" {
			medicine = line
		}
	}
	
	return replacements, medicine
}

func generateMolecules(replacements []Replacement, medicine string) map[string]bool {
	molecules := make(map[string]bool)
	
	for _, rep := range replacements {
		for i := 0; i < len(medicine); i++ {
			if strings.HasPrefix(medicine[i:], rep.from) {
				newMolecule := medicine[:i] + rep.to + medicine[i+len(rep.from):]
				molecules[newMolecule] = true
			}
		}
	}
	
	return molecules
}

func findMinSteps(replacements []Replacement, target string) int {
	minSteps := len(target) // Upper bound

	for i := 0; i < 1000; i++ { // Try 1000 times
		shuffleReplacements(replacements)
		steps := 0
		molecule := target

		for molecule != "e" {
			prevMolecule := molecule
			for _, rep := range replacements {
				if strings.Contains(molecule, rep.to) {
					molecule = strings.Replace(molecule, rep.to, rep.from, 1)
					steps++
					break
				}
			}
			if prevMolecule == molecule {
				break // Stuck, try again
			}
		}

		if molecule == "e" && steps < minSteps {
			minSteps = steps
		}
	}

	return minSteps
}

func shuffleReplacements(replacements []Replacement) {
	rand.Shuffle(len(replacements), func(i, j int) {
		replacements[i], replacements[j] = replacements[j], replacements[i]
	})
}
