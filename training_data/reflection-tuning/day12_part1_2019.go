package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Moon struct {
	pos, vel [3]int
}

func (m *Moon) updateVelocity(other *Moon) {
	for i := 0; i < 3; i++ {
		if m.pos[i] < other.pos[i] {
			m.vel[i]++
			other.vel[i]--
		} else if m.pos[i] > other.pos[i] {
			m.vel[i]--
			other.vel[i]++
		}
	}
}

func (m *Moon) updatePosition() {
	for i := 0; i < 3; i++ {
		m.pos[i] += m.vel[i]
	}
}

func (m *Moon) energy() int {
	pot, kin := 0, 0
	for i := 0; i < 3; i++ {
		pot += abs(m.pos[i])
		kin += abs(m.vel[i])
	}
	return pot * kin
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func simulateMotion(moons []*Moon, steps int) {
	for step := 0; step < steps; step++ {
		for i := 0; i < len(moons); i++ {
			for j := i + 1; j < len(moons); j++ {
				moons[i].updateVelocity(moons[j])
			}
		}
		for _, moon := range moons {
			moon.updatePosition()
		}
	}
}

func totalEnergy(moons []*Moon) int {
	total := 0
	for _, moon := range moons {
		total += moon.energy()
	}
	return total
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var moons []*Moon
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(strings.Trim(line, "<>"), ", ")
		moon := &Moon{}
		for i, part := range parts {
			value, _ := strconv.Atoi(strings.Split(part, "=")[1])
			moon.pos[i] = value
		}
		moons = append(moons, moon)
	}

	simulateMotion(moons, 1000)
	energy := totalEnergy(moons)
	fmt.Println("Total energy after 1000 steps:", energy)
}
