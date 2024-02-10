package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	happinessMap, err := readHappinessValues("input.txt")
	if err != nil {
		fmt.Println("Error reading input:", err)
		return
	}

	guests := getGuestList(happinessMap)
	maxHappiness := calculateOptimalArrangement(guests, happinessMap)
	fmt.Println(maxHappiness)
}

func readHappinessValues(filename string) (map[string]map[string]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	happinessMap := make(map[string]map[string]int)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) < 11 {
			continue // Skip invalid lines
		}
		from, to := parts[0], parts[10][:len(parts[10])-1] // Trim period
		change, err := strconv.Atoi(parts[3])
		if err != nil {
			return nil, err
		}
		if parts[2] == "lose" {
			change = -change
		}

		if happinessMap[from] == nil {
			happinessMap[from] = make(map[string]int)
		}
		happinessMap[from][to] = change
	}

	return happinessMap, scanner.Err()
}

func getGuestList(happinessMap map[string]map[string]int) []string {
	guests := make([]string, 0, len(happinessMap))
	for guest := range happinessMap {
		guests = append(guests, guest)
	}
	return guests
}

func calculateOptimalArrangement(guests []string, happinessMap map[string]map[string]int) int {
	maxHappiness := 0
	permute(guests, 0, &maxHappiness, happinessMap)
	return maxHappiness
}

func permute(arr []string, i int, maxHappiness *int, happinessMap map[string]map[string]int) {
	if i > len(arr) {
		return
	}
	if i == len(arr) {
		happiness := calculateHappiness(arr, happinessMap)
		if happiness > *maxHappiness {
			*maxHappiness = happiness
		}
		return
	}
	for j := i; j < len(arr); j++ {
		arr[i], arr[j] = arr[j], arr[i]
		permute(arr, i+1, maxHappiness, happinessMap)
		arr[i], arr[j] = arr[j], arr[i]
	}
}

func calculateHappiness(arrangement []string, happinessMap map[string]map[string]int) int {
	happiness := 0
	n := len(arrangement)
	for i := 0; i < n; i++ {
		left := (i + n - 1) % n
		right := (i + 1) % n
		happiness += happinessMap[arrangement[i]][arrangement[left]]
		happiness += happinessMap[arrangement[i]][arrangement[right]]
	}
	return happiness
}