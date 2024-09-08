package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

type Vector3D struct {
	x, y, z int
}

type Moon struct {
	position Vector3D
	velocity Vector3D
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func lcm(a, b int) int {
	return a * b / gcd(a, b)
}

func updateVelocity(moons []Moon) {
	for i := range moons {
		for j := i + 1; j < len(moons); j++ {
			for axis := 0; axis < 3; axis++ {
				posI := [3]int{moons[i].position.x, moons[i].position.y, moons[i].position.z}[axis]
				posJ := [3]int{moons[j].position.x, moons[j].position.y, moons[j].position.z}[axis]
				velI := &[3]*int{&moons[i].velocity.x, &moons[i].velocity.y, &moons[i].velocity.z}[axis]
				velJ := &[3]*int{&moons[j].velocity.x, &moons[j].velocity.y, &moons[j].velocity.z}[axis]

				if posI < posJ {
					*velI++
					*velJ--
				} else if posI > posJ {
					*velI--
					*velJ++
				}
			}
		}
	}
}

func updatePosition(moons []Moon) {
	for i := range moons {
		moons[i].position.x += moons[i].velocity.x
		moons[i].position.y += moons[i].velocity.y
		moons[i].position.z += moons[i].velocity.z
	}
}

func calculateEnergy(moons []Moon) int {
	totalEnergy := 0
	for _, moon := range moons {
		potential := abs(moon.position.x) + abs(moon.position.y) + abs(moon.position.z)
		kinetic := abs(moon.velocity.x) + abs(moon.velocity.y) + abs(moon.velocity.z)
		totalEnergy += potential * kinetic
	}
	return totalEnergy
}

func findPeriod(moons []Moon) int {
	initial := make([]Moon, len(moons))
	copy(initial, moons)

	periods := [3]int{0, 0, 0}
	steps := 0

	for periods[0] == 0 || periods[1] == 0 || periods[2] == 0 {
		updateVelocity(moons)
		updatePosition(moons)
		steps++

		for axis := 0; axis < 3; axis++ {
			if periods[axis] == 0 {
				match := true
				for i := range moons {
					posMatch := [3]bool{moons[i].position.x == initial[i].position.x,
						moons[i].position.y == initial[i].position.y,
						moons[i].position.z == initial[i].position.z}[axis]
					velMatch := [3]bool{moons[i].velocity.x == initial[i].velocity.x,
						moons[i].velocity.y == initial[i].velocity.y,
						moons[i].velocity.z == initial[i].velocity.z}[axis]
					if !posMatch || !velMatch {
						match = false
						break
					}
				}
				if match {
					periods[axis] = steps
				}
			}
		}
	}

	return lcm(lcm(periods[0], periods[1]), periods[2])
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var moons []Moon
	re := regexp.MustCompile(`<x=(-?\d+), y=(-?\d+), z=(-?\d+)>`)

	for scanner.Scan() {
		matches := re.FindStringSubmatch(scanner.Text())
		x, _ := strconv.Atoi(matches[1])
		y, _ := strconv.Atoi(matches[2])
		z, _ := strconv.Atoi(matches[3])
		moons = append(moons, Moon{position: Vector3D{x, y, z}})
	}

	// Part 1
	moonsCopy := make([]Moon, len(moons))
	copy(moonsCopy, moons)
	for i := 0; i < 1000; i++ {
		updateVelocity(moonsCopy)
		updatePosition(moonsCopy)
	}
	fmt.Println("Part 1:", calculateEnergy(moonsCopy))

	// Part 2
	fmt.Println("Part 2:", findPeriod(moons))
}
