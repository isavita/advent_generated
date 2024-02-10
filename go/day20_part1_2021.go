package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	algorithm, image := readInput("input.txt")
	image = enhanceImage(image, algorithm, 2)
	fmt.Println(countLitPixels(image))
}

func readInput(filename string) (string, [][]rune) {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	algorithm := strings.ReplaceAll(scanner.Text(), "\n", "")
	scanner.Scan() // Skip the empty line

	var image [][]rune
	for scanner.Scan() {
		image = append(image, []rune(scanner.Text()))
	}

	return algorithm, image
}

func enhanceImage(image [][]rune, algorithm string, times int) [][]rune {
	for i := 0; i < times; i++ {
		image = applyAlgorithm(image, algorithm, i%2 == 1 && algorithm[0] == '#')
	}
	return image
}

func applyAlgorithm(image [][]rune, algorithm string, flip bool) [][]rune {
	enhancedImage := make([][]rune, len(image)+2)
	for i := range enhancedImage {
		enhancedImage[i] = make([]rune, len(image[0])+2)
		for j := range enhancedImage[i] {
			index := calculateIndex(i-1, j-1, image, flip)
			enhancedImage[i][j] = rune(algorithm[index])
		}
	}
	return enhancedImage
}

func calculateIndex(i, j int, image [][]rune, flip bool) int {
	index := 0
	for di := -1; di <= 1; di++ {
		for dj := -1; dj <= 1; dj++ {
			index <<= 1
			if i+di >= 0 && i+di < len(image) && j+dj >= 0 && j+dj < len(image[0]) {
				if image[i+di][j+dj] == '#' {
					index |= 1
				}
			} else if flip {
				index |= 1
			}
		}
	}
	return index
}

func countLitPixels(image [][]rune) (count int) {
	for _, row := range image {
		for _, pixel := range row {
			if pixel == '#' {
				count++
			}
		}
	}
	return count
}