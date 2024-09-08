package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Vector struct {
	x, y, z int
}

type Particle struct {
	p, v, a Vector
	index   int
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func manhattanDistance(v Vector) int {
	return abs(v.x) + abs(v.y) + abs(v.z)
}

func parseVector(s string) Vector {
	parts := strings.Split(strings.Trim(s, "<>"), ",")
	x, _ := strconv.Atoi(parts[0])
	y, _ := strconv.Atoi(parts[1])
	z, _ := strconv.Atoi(parts[2])
	return Vector{x, y, z}
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var particles []Particle
	index := 0

	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), ", ")
		p := parseVector(strings.TrimPrefix(parts[0], "p="))
		v := parseVector(strings.TrimPrefix(parts[1], "v="))
		a := parseVector(strings.TrimPrefix(parts[2], "a="))
		particles = append(particles, Particle{p, v, a, index})
		index++
	}

	closestParticle := 0
	minAccel := math.MaxInt32
	minVel := math.MaxInt32
	minPos := math.MaxInt32

	for _, particle := range particles {
		accel := manhattanDistance(particle.a)
		vel := manhattanDistance(particle.v)
		pos := manhattanDistance(particle.p)

		if accel < minAccel || (accel == minAccel && vel < minVel) || (accel == minAccel && vel == minVel && pos < minPos) {
			closestParticle = particle.index
			minAccel = accel
			minVel = vel
			minPos = pos
		}
	}

	fmt.Println(closestParticle)
}
