package main

import (
	_ "embed"
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))
	result := solve(input)
	fmt.Println(result)

}

func part1(input string) int {
	cubes := parseInput(input)

	onCoords := map[[3]int]bool{}

	for _, c := range cubes {
		if part1OutOfBounds(c.x1, c.x2, c.y1, c.y2, c.z1, c.z2) {
			continue
		}

		for x := c.x1; x <= c.x2; x++ {
			for y := c.y1; y <= c.y2; y++ {
				for z := c.z1; z <= c.z2; z++ {
					coord := [3]int{x, y, z}
					onCoords[coord] = c.isOn
				}
			}
		}
	}

	var count int
	for _, b := range onCoords {
		if b {
			count++
		}
	}

	return count
}

func part1OutOfBounds(nums ...int) bool {
	for _, n := range nums {
		if n < -50 || n > 50 {
			return true
		}
	}
	return false
}

func solve(input string) int {
	cubes := parseInput(input)

	var finalList []cube
	for _, c := range cubes {
		var toAdd []cube

		for _, finalCube := range finalList {
			intersection, didIntersect := finalCube.getIntersection(c)
			if didIntersect {
				toAdd = append(toAdd, intersection)
			}
		}

		if c.isOn {
			toAdd = append(toAdd, c)
		}

		finalList = append(finalList, toAdd...)
	}

	var total int
	for _, c := range finalList {
		total += c.volume()
	}

	return total
}

type cube struct {
	isOn   bool
	x1, x2 int
	y1, y2 int
	z1, z2 int
}

func (c cube) getIntersection(c2 cube) (intersection cube, hasIntersection bool) {
	x1 := maxInt(c.x1, c2.x1)
	x2 := minInt(c.x2, c2.x2)
	y1 := maxInt(c.y1, c2.y1)
	y2 := minInt(c.y2, c2.y2)
	z1 := maxInt(c.z1, c2.z1)
	z2 := minInt(c.z2, c2.z2)

	if x1 > x2 || y1 > y2 || z1 > z2 {
		return cube{}, false
	}

	var intersectionState bool
	if c.isOn && c2.isOn {
		intersectionState = false
	} else if !c.isOn && !c2.isOn {
		intersectionState = true
	} else {

		intersectionState = c2.isOn
	}

	return cube{
		isOn: intersectionState,
		x1:   x1, x2: x2,
		y1: y1, y2: y2,
		z1: z1, z2: z2,
	}, true
}

func (c cube) volume() int {
	vol := (c.x2 - c.x1 + 1) * (c.y2 - c.y1 + 1) * (c.z2 - c.z1 + 1)
	if c.isOn {
		return vol
	}
	return -vol
}

func parseInput(input string) (ans []cube) {
	for _, line := range strings.Split(input, "\n") {

		parts := strings.Split(line, " ")

		var x1, x2, y1, y2, z1, z2 int
		n, err := fmt.Sscanf(parts[1], "x=%d..%d,y=%d..%d,z=%d..%d", &x1, &x2, &y1, &y2, &z1, &z2)
		if err != nil || n != 6 {
			panic(fmt.Sprintf("parsing error %v, vals parsed %d", err, n))
		}

		if x1 > x2 || y1 > y2 || z1 > z2 {

			panic("didn't expect input to have backwards coords, sort them...")
		}

		ans = append(ans, cube{
			isOn: parts[0] == "on",
			x1:   x1,
			x2:   x2,
			y1:   y1,
			y2:   y2,
			z1:   z1,
			z2:   z2,
		})
	}
	return ans
}

func maxInt(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func minInt(a, b int) int {
	return -maxInt(-a, -b)
}