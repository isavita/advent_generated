package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"strconv"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	const width, height = 101, 103
	robots := [][]int{}
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " ")
		pPart := strings.TrimPrefix(parts[0], "p=")
		vPart := strings.TrimPrefix(parts[1], "v=")
		pos := strings.Split(pPart, ",")
		vel := strings.Split(vPart, ",")
		px, _ := strconv.Atoi(pos[0])
		py, _ := strconv.Atoi(pos[1])
		vx, _ := strconv.Atoi(vel[0])
		vy, _ := strconv.Atoi(vel[1])
		robots = append(robots, []int{px, py, vx, vy})
	}

	for i := 0; i < 100; i++ {
		for j := range robots {
			r := robots[j]
			x := (r[0] + r[2]) % width
			y := (r[1] + r[3]) % height
			if x < 0 { x += width }
			if y < 0 { y += height }
			robots[j][0] = x
			robots[j][1] = y
		}
	}

	q1, q2, q3, q4 := 0, 0, 0, 0
	for _, r := range robots {
		x, y := r[0], r[1]
		if x == 50 || y == 51 { continue }
		if x < 50 && y < 51 { q1++ }
		if x > 50 && y < 51 { q2++ }
		if x < 50 && y > 51 { q3++ }
		if x > 50 && y > 51 { q4++ }
	}

	fmt.Println(q1 * q2 * q3 * q4)
}
