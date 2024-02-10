package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func filterValues(values []string, criteria func(zeros, ones int) byte) string {
	for i := 0; i < len(values[0]); i++ {
		zeros, ones := 0, 0
		for _, val := range values {
			if val[i] == '0' {
				zeros++
			} else {
				ones++
			}
		}
		keep := criteria(zeros, ones)
		values = filterByBit(values, i, keep)
		if len(values) == 1 {
			break
		}
	}
	return values[0]
}

func filterByBit(values []string, bitIndex int, keep byte) []string {
	filtered := make([]string, 0)
	for _, val := range values {
		if val[bitIndex] == keep {
			filtered = append(filtered, val)
		}
	}
	return filtered
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var values []string
	for scanner.Scan() {
		values = append(values, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	oxygenGeneratorRating := filterValues(values, func(zeros, ones int) byte {
		if zeros > ones {
			return '0'
		} else {
			return '1'
		}
	})
	oxygenGeneratorRatingInt, err := strconv.ParseInt(oxygenGeneratorRating, 2, 64)
	if err != nil {
		panic(err)
	}

	co2ScrubberRating := filterValues(values, func(zeros, ones int) byte {
		if zeros <= ones {
			return '0'
		} else {
			return '1'
		}
	})
	co2ScrubberRatingInt, err := strconv.ParseInt(co2ScrubberRating, 2, 64)
	if err != nil {
		panic(err)
	}

	fmt.Println(oxygenGeneratorRatingInt * co2ScrubberRatingInt)
}