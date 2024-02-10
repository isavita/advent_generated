package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	distances, err := readAndParseInput("input.txt")
	if err != nil {
		fmt.Println("Error reading input:", err)
		return
	}

	locations := getUniqueLocations(distances)
	maxDistance := findLongestRoute(locations, distances)
	fmt.Println(maxDistance)
}

func readAndParseInput(filename string) (map[string]map[string]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	distances := make(map[string]map[string]int)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), " ")
		if len(parts) != 5 {
			continue // Invalid line format
		}

		from, to, dist := parts[0], parts[2], parts[4]
		distance, err := strconv.Atoi(dist)
		if err != nil {
			return nil, err
		}

		if distances[from] == nil {
			distances[from] = make(map[string]int)
		}
		distances[from][to] = distance

		if distances[to] == nil {
			distances[to] = make(map[string]int)
		}
		distances[to][from] = distance // Assuming distance is symmetric
	}

	return distances, scanner.Err()
}

func getUniqueLocations(distances map[string]map[string]int) []string {
	locationSet := make(map[string]struct{})
	for from := range distances {
		locationSet[from] = struct{}{}
		for to := range distances[from] {
			locationSet[to] = struct{}{}
		}
	}

	var locations []string
	for location := range locationSet {
		locations = append(locations, location)
	}

	return locations
}

func findLongestRoute(locations []string, distances map[string]map[string]int) int {
	maxDistance := 0
	permute(locations, 0, &maxDistance, distances, false)
	return maxDistance
}

func permute(arr []string, i int, bestDistance *int, distances map[string]map[string]int, findShortest bool) {
	if i > len(arr) {
		return
	}
	if i == len(arr) {
		dist := calculateRouteDistance(arr, distances)
		if findShortest {
			if *bestDistance == 0 || dist < *bestDistance {
				*bestDistance = dist
			}
		} else {
			if dist > *bestDistance {
				*bestDistance = dist
			}
		}
		return
	}
	for j := i; j < len(arr); j++ {
		arr[i], arr[j] = arr[j], arr[i]
		permute(arr, i+1, bestDistance, distances, findShortest)
		arr[i], arr[j] = arr[j], arr[i]
	}
}

func calculateRouteDistance(route []string, distances map[string]map[string]int) int {
	sum := 0
	for i := 0; i < len(route)-1; i++ {
		sum += distances[route[i]][route[i+1]]
	}
	return sum
}