package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
)

type Point struct {
	x, y, z int
}

type Scanner struct {
	beacons []Point
	pos     Point
}

func (p Point) sub(q Point) Point {
	return Point{p.x - q.x, p.y - q.y, p.z - q.z}
}

func (p Point) add(q Point) Point {
	return Point{p.x + q.x, p.y + q.y, p.z + q.z}
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func rotations() []func(Point) Point {
	return []func(Point) Point{
		func(p Point) Point { return Point{p.x, p.y, p.z} },
		func(p Point) Point { return Point{p.x, p.z, -p.y} },
		func(p Point) Point { return Point{p.x, -p.y, -p.z} },
		func(p Point) Point { return Point{p.x, -p.z, p.y} },
		func(p Point) Point { return Point{-p.x, p.y, -p.z} },
		func(p Point) Point { return Point{-p.x, p.z, p.y} },
		func(p Point) Point { return Point{-p.x, -p.y, p.z} },
		func(p Point) Point { return Point{-p.x, -p.z, -p.y} },
		func(p Point) Point { return Point{p.y, p.z, p.x} },
		func(p Point) Point { return Point{p.y, p.x, -p.z} },
		func(p Point) Point { return Point{p.y, -p.z, -p.x} },
		func(p Point) Point { return Point{p.y, -p.x, p.z} },
		func(p Point) Point { return Point{-p.y, p.z, -p.x} },
		func(p Point) Point { return Point{-p.y, p.x, p.z} },
		func(p Point) Point { return Point{-p.y, -p.z, p.x} },
		func(p Point) Point { return Point{-p.y, -p.x, -p.z} },
		func(p Point) Point { return Point{p.z, p.x, p.y} },
		func(p Point) Point { return Point{p.z, p.y, -p.x} },
		func(p Point) Point { return Point{p.z, -p.x, -p.y} },
		func(p Point) Point { return Point{p.z, -p.y, p.x} },
		func(p Point) Point { return Point{-p.z, p.x, -p.y} },
		func(p Point) Point { return Point{-p.z, p.y, p.x} },
		func(p Point) Point { return Point{-p.z, -p.x, p.y} },
		func(p Point) Point { return Point{-p.z, -p.y, -p.x} },
	}
}

func findOverlap(s1, s2 Scanner) (bool, Point, func(Point) Point) {
	for _, rot := range rotations() {
		diffs := make(map[Point]int)
		for _, b1 := range s1.beacons {
			for _, b2 := range s2.beacons {
				diff := b1.sub(rot(b2))
				diffs[diff]++
				if diffs[diff] >= 12 {
					return true, diff, rot
				}
			}
		}
	}
	return false, Point{}, nil
}

func parseInput(filename string) []Scanner {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)
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
	scanners = append(scanners, currentScanner)
	return scanners
}

func main() {
	scanners := parseInput("input.txt")
	aligned := make([]bool, len(scanners))
	aligned[0] = true
	scanners[0].pos = Point{0, 0, 0}

	var wg sync.WaitGroup
	for len(scanners) > 1 {
		for i := 1; i < len(scanners); i++ {
			if aligned[i] {
				continue
			}
			wg.Add(1)
			go func(i int) {
				defer wg.Done()
				for j := 0; j < len(scanners); j++ {
					if !aligned[j] || i == j {
						continue
					}
					if ok, diff, rot := findOverlap(scanners[j], scanners[i]); ok {
						aligned[i] = true
						scanners[i].pos = scanners[j].pos.add(diff)
						for k, beacon := range scanners[i].beacons {
							scanners[i].beacons[k] = rot(beacon).add(diff)
						}
						break
					}
				}
			}(i)
		}
		wg.Wait()
	}

	beacons := make(map[Point]bool)
	for _, s := range scanners {
		for _, b := range s.beacons {
			beacons[b] = true
		}
	}

	fmt.Println(len(beacons))
}
