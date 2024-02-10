package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Particle struct {
	p, v, a [3]int
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func manhattan(x [3]int) int {
	return abs(x[0]) + abs(x[1]) + abs(x[2])
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var particles []Particle
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ", ")

		var p Particle
		for i, part := range parts {
			coords := strings.Split(part[3:len(part)-1], ",")
			for j, coord := range coords {
				num, _ := strconv.Atoi(coord)
				switch i {
				case 0:
					p.p[j] = num
				case 1:
					p.v[j] = num
				case 2:
					p.a[j] = num
				}
			}
		}
		particles = append(particles, p)
	}

	var closestParticle int
	minAccel := math.MaxInt64
	minVelocity := math.MaxInt64
	minPosition := math.MaxInt64

	for i, particle := range particles {
		accel := manhattan(particle.a)
		velocity := manhattan(particle.v)
		position := manhattan(particle.p)

		if accel < minAccel || (accel == minAccel && velocity < minVelocity) ||
			(accel == minAccel && velocity == minVelocity && position < minPosition) {
			minAccel = accel
			minVelocity = velocity
			minPosition = position
			closestParticle = i
		}
	}

	fmt.Println(closestParticle)
}