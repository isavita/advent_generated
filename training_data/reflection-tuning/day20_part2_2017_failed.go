package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Vector struct {
	X, Y, Z int
}

type Particle struct {
	Position     Vector
	Velocity     Vector
	Acceleration Vector
}

func (p *Particle) Update() {
	p.Velocity.X += p.Acceleration.X
	p.Velocity.Y += p.Acceleration.Y
	p.Velocity.Z += p.Acceleration.Z
	p.Position.X += p.Velocity.X
	p.Position.Y += p.Velocity.Y
	p.Position.Z += p.Velocity.Z
}

func (p *Particle) ManhattanDistance() int {
	return abs(p.Position.X) + abs(p.Position.Y) + abs(p.Position.Z)
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func parseVector(s string) Vector {
	parts := strings.Split(strings.Trim(s, "<>"), ",")
	x, _ := strconv.Atoi(parts[0])
	y, _ := strconv.Atoi(parts[1])
	z, _ := strconv.Atoi(parts[2])
	return Vector{x, y, z}
}

func parseParticle(line string) Particle {
	parts := strings.Split(line, ", ")
	return Particle{
		Position:     parseVector(parts[0][3:]),
		Velocity:     parseVector(parts[1][3:]),
		Acceleration: parseVector(parts[2][3:]),
	}
}

func simulateCollisions(particles []Particle) int {
	for tick := 0; tick < 1000; tick++ {
		positions := make(map[Vector][]int)
		for i := range particles {
			particles[i].Update()
			positions[particles[i].Position] = append(positions[particles[i].Position], i)
		}

		var newParticles []Particle
		for _, indices := range positions {
			if len(indices) == 1 {
				newParticles = append(newParticles, particles[indices[0]])
			}
		}
		particles = newParticles

		if len(particles) == len(newParticles) {
			break
		}
	}
	return len(particles)
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var particles []Particle
	for scanner.Scan() {
		particles = append(particles, parseParticle(scanner.Text()))
	}

	// Part 1
	closestParticle := 0
	minAcceleration := particles[0].Acceleration.X*particles[0].Acceleration.X +
		particles[0].Acceleration.Y*particles[0].Acceleration.Y +
		particles[0].Acceleration.Z*particles[0].Acceleration.Z

	for i, p := range particles {
		acceleration := p.Acceleration.X*p.Acceleration.X +
			p.Acceleration.Y*p.Acceleration.Y +
			p.Acceleration.Z*p.Acceleration.Z
		if acceleration < minAcceleration {
			minAcceleration = acceleration
			closestParticle = i
		}
	}
	fmt.Println("Part 1:", closestParticle)

	// Part 2
	remainingParticles := simulateCollisions(particles)
	fmt.Println("Part 2:", remainingParticles)
}
