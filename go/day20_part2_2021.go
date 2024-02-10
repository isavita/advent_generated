package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

const (
	iterations = 50 // Number of times the algorithm should be applied
	expandBy   = 1  // How much the image expands with each iteration
)

func main() {
	algorithm, image := readInput("input.txt")
	for i := 0; i < iterations; i++ {
		image = enhanceImage(algorithm, image, i%2 == 1 && algorithm[0] == '#')
	}
	fmt.Println(countLitPixels(image))
}

func readInput(filename string) (algorithm string, image [][]bool) {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	algorithm = scanner.Text()

	scanner.Scan() // skip the empty line

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		row := make([]bool, len(line))
		for i, char := range line {
			row[i] = char == '#'
		}
		image = append(image, row)
	}
	return algorithm, image
}

func enhanceImage(algorithm string, image [][]bool, useInfiniteLit bool) [][]bool {
	// Create a new image with padding for the expansion
	newImage := make([][]bool, len(image)+(expandBy*2))
	for i := range newImage {
		newImage[i] = make([]bool, len(image[0])+(expandBy*2))
	}

	for y := -expandBy; y < len(image)+expandBy; y++ {
		for x := -expandBy; x < len(image[0])+expandBy; x++ {
			index := 0
			for dy := -1; dy <= 1; dy++ {
				for dx := -1; dx <= 1; dx++ {
					index <<= 1
					ny, nx := y+dy, x+dx
					if ny >= 0 && ny < len(image) && nx >= 0 && nx < len(image[0]) {
						if image[ny][nx] {
							index |= 1
						}
					} else if useInfiniteLit {
						index |= 1
					}
				}
			}
			newImage[y+expandBy][x+expandBy] = algorithm[index] == '#'
		}
	}
	return newImage
}

func countLitPixels(image [][]bool) (count int) {
	for _, row := range image {
		for _, pixel := range row {
			if pixel {
				count++
			}
		}
	}
	return count
}