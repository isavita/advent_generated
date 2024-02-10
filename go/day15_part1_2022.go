package main

import (
	"fmt"
	"image"
	"os"
	"strings"
)

type sensor struct {
	pos    image.Point
	beacon image.Point
	dist   int
}

func main() {
	var sensors []sensor
	input := readAll("input.txt")
	for _, line := range strings.Split(input, "\n") {
		var s sensor
		fmt.Sscanf(line, "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d", &s.pos.X, &s.pos.Y, &s.beacon.X, &s.beacon.Y)
		s.dist = manhattan(s.pos, s.beacon)
		sensors = append(sensors, s)
	}
	fmt.Println(impossible(sensors, 2000000))
}

func impossible(sensors []sensor, y int) int {
	pts := setOf([]int{})
	for _, s := range sensors {
		dist := s.dist - abs(s.pos.Y-y)
		for x := 0; x <= dist; x++ {
			pts[s.pos.X+x] = struct{}{}
			pts[s.pos.X-x] = struct{}{}
		}
	}
	for _, s := range sensors {
		if s.beacon.Y == y {
			delete(pts, s.beacon.X)
		}
	}
	return len(pts)
}

func readAll(path string) string {
	file, err := os.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return string(file)
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func manhattan(p, q image.Point) int {
	return abs(p.X-q.X) + abs(p.Y-q.Y)
}

type Set[T comparable] map[T]struct{}

func setOf[T comparable](a []T) Set[T] {
	s := make(Set[T])
	for _, c := range a {
		s[c] = struct{}{}
	}
	return s
}