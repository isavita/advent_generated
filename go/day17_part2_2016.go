package main

import (
	"bufio"
	"crypto/md5"
	"fmt"
	"io"
	"os"
)

type Point struct {
	x, y int
	path string
}

func main() {
	passcode := readPasscode("input.txt")
	longestPathLength := findLongestPathLength(passcode)
	fmt.Println(longestPathLength)
}

func readPasscode(filename string) string {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	if scanner.Scan() {
		return scanner.Text()
	}

	panic("Failed to read passcode")
}

func findLongestPathLength(passcode string) int {
	longest := 0
	queue := []Point{{0, 0, ""}}
	for len(queue) > 0 {
		point := queue[0]
		queue = queue[1:]

		if point.x == 3 && point.y == 3 {
			if len(point.path) > longest {
				longest = len(point.path)
			}
			continue
		}

		for _, dir := range getOpenDoors(passcode, point.path) {
			nextPoint := Point{point.x, point.y, point.path + dir}
			switch dir {
			case "U":
				nextPoint.y--
			case "D":
				nextPoint.y++
			case "L":
				nextPoint.x--
			case "R":
				nextPoint.x++
			}

			if nextPoint.x >= 0 && nextPoint.x < 4 && nextPoint.y >= 0 && nextPoint.y < 4 {
				queue = append(queue, nextPoint)
			}
		}
	}
	return longest
}

func getOpenDoors(passcode, path string) []string {
	hash := md5Hash(passcode + path)
	doors := []string{}
	if hash[0] >= 'b' && hash[0] <= 'f' {
		doors = append(doors, "U")
	}
	if hash[1] >= 'b' && hash[1] <= 'f' {
		doors = append(doors, "D")
	}
	if hash[2] >= 'b' && hash[2] <= 'f' {
		doors = append(doors, "L")
	}
	if hash[3] >= 'b' && hash[3] <= 'f' {
		doors = append(doors, "R")
	}
	return doors
}

func md5Hash(input string) string {
	h := md5.New()
	io.WriteString(h, input)
	return fmt.Sprintf("%x", h.Sum(nil))
}