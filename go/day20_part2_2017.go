package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Particle struct {
	p, v, a [3]int
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

	for tick := 0; tick < 1000; tick++ {
		positions := make(map[string]int)
		for i, particle := range particles {
			for j := 0; j < 3; j++ {
				particle.v[j] += particle.a[j]
				particle.p[j] += particle.v[j]
			}
			particles[i] = particle
			posStr := fmt.Sprintf("%d,%d,%d", particle.p[0], particle.p[1], particle.p[2])
			positions[posStr]++
		}

		var newParticles []Particle
		for _, particle := range particles {
			posStr := fmt.Sprintf("%d,%d,%d", particle.p[0], particle.p[1], particle.p[2])
			if positions[posStr] == 1 {
				newParticles = append(newParticles, particle)
			}
		}
		particles = newParticles
	}

	fmt.Println(len(particles))
}