package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x, y, z int
}

type Scanner struct {
	beacons []Point
}

func (p Point) sub(other Point) Point {
	return Point{p.x - other.x, p.y - other.y, p.z - other.z}
}

func (p Point) add(other Point) Point {
	return Point{p.x + other.x, p.y + other.y, p.z + other.z}
}

var rotations = []func(Point) Point{
	func(p Point) Point { return Point{p.x, p.y, p.z} },
	func(p Point) Point { return Point{p.x, -p.z, p.y} },
	func(p Point) Point { return Point{p.x, -p.y, -p.z} },
	func(p Point) Point { return Point{p.x, p.z, -p.y} },
	func(p Point) Point { return Point{-p.x, -p.y, p.z} },
	func(p Point) Point { return Point{-p.x, -p.z, -p.y} },
	func(p Point) Point { return Point{-p.x, p.y, -p.z} },
	func(p Point) Point { return Point{-p.x, p.z, p.y} },
	func(p Point) Point { return Point{p.y, p.z, p.x} },
	func(p Point) Point { return Point{p.y, -p.x, p.z} },
	func(p Point) Point { return Point{p.y, -p.z, -p.x} },
	func(p Point) Point { return Point{p.y, p.x, -p.z} },
	func(p Point) Point { return Point{-p.y, -p.z, p.x} },
	func(p Point) Point { return Point{-p.y, -p.x, -p.z} },
	func(p Point) Point { return Point{-p.y, p.z, -p.x} },
	func(p Point) Point { return Point{-p.y, p.x, p.z} },
	func(p Point) Point { return Point{p.z, p.x, p.y} },
	func(p Point) Point { return Point{p.z, -p.y, p.x} },
	func(p Point) Point { return Point{p.z, -p.x, -p.y} },
	func(p Point) Point { return Point{p.z, p.y, -p.x} },
	func(p Point) Point { return Point{-p.z, -p.y, -p.x} },
	func(p Point) Point { return Point{-p.z, -p.x, p.y} },
	func(p Point) Point { return Point{-p.z, p.y, p.x} },
	func(p Point) Point { return Point{-p.z, p.x, -p.y} },
}

func findOverlap(s1, s2 Scanner) (bool, Point) {
	for _, rot := range rotations {
		counts := make(map[Point]int)
		for _, b1 := range s1.beacons {
			for _, b2 := range s2.beacons {
				diff := b1.sub(rot(b2))
				counts[diff]++
				if counts[diff] >= 12 {
					return true, diff
				}
			}
		}
	}
	return false, Point{}
}

func main() {
	scanners := parseInput()
	aligned := make([]Scanner, len(scanners))
	aligned[0] = scanners[0]
	positions := make([]Point, len(scanners))
	toAlign := make([]int, len(scanners)-1)
	for i := range toAlign {
		toAlign[i] = i + 1
	}

	for len(toAlign) > 0 {
		for i, s := range toAlign {
			for j, a := range aligned {
				if a.beacons == nil {
					continue
				}
				if overlap, diff := findOverlap(a, scanners[s]); overlap {
					aligned[s] = Scanner{make([]Point, len(scanners[s].beacons))}
					for k, b := range scanners[s].beacons {
						for _, rot := range rotations {
							if _, ok := findOverlap(Scanner{[]Point{rot(b)}}, a); ok {
								aligned[s].beacons[k] = rot(b).add(diff)
								break
							}
						}
					}
					positions[s] = diff.add(positions[j])
					toAlign = append(toAlign[:i], toAlign[i+1:]...)
					break
				}
			}
			if aligned[s].beacons != nil {
				break
			}
		}
	}

	beacons := make(map[Point]bool)
	for _, s := range aligned {
		for _, b := range s.beacons {
			beacons[b] = true
		}
	}

	fmt.Println(len(beacons))
}

func parseInput() []Scanner {
	scanner := bufio.NewScanner(os.Stdin)
	var scanners []Scanner
	var currentScanner Scanner

	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "---") {
			if len(currentScanner.beacons) > 0 {
				scanners = append(scanners, currentScanner)
			}
			currentScanner = Scanner{}
		} else if line != "" {
			coords := strings.Split(line, ",")
			x, _ := strconv.Atoi(coords[0])
			y, _ := strconv.Atoi(coords[1])
			z, _ := strconv.Atoi(coords[2])
			currentScanner.beacons = append(currentScanner.beacons, Point{x, y, z})
		}
	}
	if len(currentScanner.beacons) > 0 {
		scanners = append(scanners, currentScanner)
	}
	return scanners
}
