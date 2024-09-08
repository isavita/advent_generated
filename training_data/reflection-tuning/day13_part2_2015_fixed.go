package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"strconv"
)

func main() {
	happiness := make(map[string]map[string]int)
	people := make(map[string]bool)

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Fields(line)
		person1 := parts[0]
		person2 := parts[10][:len(parts[10])-1] // Remove the trailing period
		value, _ := strconv.Atoi(parts[3])
		if parts[2] == "lose" {
			value = -value
		}

		if happiness[person1] == nil {
			happiness[person1] = make(map[string]int)
		}
		happiness[person1][person2] = value
		people[person1] = true
		people[person2] = true
	}

	// Part 1
	maxHappiness := findMaxHappiness(happiness, people)
	fmt.Println("Part 1:", maxHappiness)

	// Part 2
	for person := range people {
		if happiness["you"] == nil {
			happiness["you"] = make(map[string]int)
		}
		happiness["you"][person] = 0
		happiness[person]["you"] = 0
	}
	people["you"] = true

	maxHappinessWithYou := findMaxHappiness(happiness, people)
	fmt.Println("Part 2:", maxHappinessWithYou)
}

func findMaxHappiness(happiness map[string]map[string]int, people map[string]bool) int {
	var peopleList []string
	for person := range people {
		peopleList = append(peopleList, person)
	}

	maxHappiness := 0
	permute(peopleList, func(arrangement []string) {
		totalHappiness := 0
		for i := 0; i < len(arrangement); i++ {
			next := (i + 1) % len(arrangement)
			totalHappiness += happiness[arrangement[i]][arrangement[next]]
			totalHappiness += happiness[arrangement[next]][arrangement[i]]
		}
		if totalHappiness > maxHappiness {
			maxHappiness = totalHappiness
		}
	})

	return maxHappiness
}

func permute(arr []string, f func([]string)) {
	permuteHelper(arr, f, 0)
}

func permuteHelper(arr []string, f func([]string), i int) {
	if i > len(arr) {
		f(arr)
		return
	}
	permuteHelper(arr, f, i+1)
	for j := i + 1; j < len(arr); j++ {
		arr[i], arr[j] = arr[j], arr[i]
		permuteHelper(arr, f, i+1)
		arr[i], arr[j] = arr[j], arr[i]
	}
}
