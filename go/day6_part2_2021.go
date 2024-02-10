package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	defer file.Close()
	if err != nil {
		panic(err)
	}

	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)

	var lanternFishCounts [9]int

	for scanner.Scan() {
		line := scanner.Text()
		fishAges := strings.Split(line, ",")
		for _, age := range fishAges {
			ageCount, _ := strconv.Atoi(age)
			lanternFishCounts[ageCount]++
		}
	}

	for i := 0; i < 256; i++ {
		newLanternFish := lanternFishCounts[0]
		for j := 0; j < len(lanternFishCounts)-1; j++ {
			lanternFishCounts[j] = lanternFishCounts[j+1]
		}
		lanternFishCounts[6] += newLanternFish
		lanternFishCounts[8] = newLanternFish
	}

	fmt.Println(sum(lanternFishCounts))
}

func sum(numbers [9]int) int {
	sum := 0
	for _, num := range numbers {
		sum += num
	}
	return sum
}