package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))

	result := solve(input)
	fmt.Println(result)
}

func solve(input string) int {
	scanners := parseInput(input)

	settled := []scanner{scanners[0]}
	settled[0].absoluteCoords = settled[0].relativeCoords
	settled[0].fillAbsoluteCoordsMap()

	undetermined := scanners[1:]

	for len(undetermined) > 0 {
		for i, undet := range undetermined {
			maybeUpdated, ok := findAbsoluteCoordsForScanner(undet, settled)
			if ok {
				settled = append(settled, maybeUpdated)

				copy(undetermined[i:], undetermined[i+1:])

				undetermined = undetermined[:len(undetermined)-1]

				break
			}
		}
	}

	allBeacons := map[[3]int]bool{}
	for _, s := range settled {
		for c := range s.absoluteCoordsMap {
			allBeacons[c] = true
		}
	}

	return len(allBeacons)
}

type scanner struct {
	number            int
	x, y, z           int
	relativeCoords    [][3]int
	rotations         [][][3]int
	absoluteCoords    [][3]int
	absoluteCoordsMap map[[3]int]bool
}

func (s *scanner) fillAbsoluteCoordsMap() {
	s.absoluteCoordsMap = map[[3]int]bool{}
	if len(s.absoluteCoords) == 0 {
		panic(fmt.Sprintf("absolute coords not set for scanner %d", s.number))
	}
	for _, ac := range s.absoluteCoords {
		s.absoluteCoordsMap[ac] = true
	}
}

func (s *scanner) fillRotations() {

	posX := s.relativeCoords
	var dir2, dir3, dir4, dir5, dir6 [][3]int
	for _, c := range posX {
		x, y, z := c[0], c[1], c[2]
		dir2 = append(dir2, [3]int{x, -y, -z})
		dir3 = append(dir3, [3]int{x, -z, y})
		dir4 = append(dir4, [3]int{-y, -z, x})
		dir5 = append(dir5, [3]int{-x, -z, -y})
		dir6 = append(dir6, [3]int{y, -z, -x})
	}
	sixRotations := [][][3]int{
		posX, dir2,
		dir3, dir4,
		dir5, dir6,
	}

	var finalRotations [][][3]int
	for _, rotation := range sixRotations {
		var r2, r3, r4 [][3]int
		for _, c := range rotation {
			x, y, z := c[0], c[1], c[2]
			r2 = append(r2, [3]int{-y, x, z})
			r3 = append(r3, [3]int{-x, -y, z})
			r4 = append(r4, [3]int{y, -x, z})
		}
		finalRotations = append(finalRotations, rotation, r2, r3, r4)
	}
	s.rotations = finalRotations
}

func findAbsoluteCoordsForScanner(undet scanner, settled []scanner) (maybeUpdated scanner, didUpdate bool) {

	for _, rotatedCoords := range undet.rotations {

		for _, set := range settled {
			for _, absCoord := range set.absoluteCoords {

				for _, relativeCoord := range rotatedCoords {

					unsettledAbsoluteCoords := makeAbsoluteCoordsList(absCoord, relativeCoord, rotatedCoords)

					var matchingCount int

					for _, ac := range unsettledAbsoluteCoords {
						if set.absoluteCoordsMap[ac] {

							matchingCount++
						}
					}

					if matchingCount >= 12 {
						undet.relativeCoords = rotatedCoords
						undet.absoluteCoords = unsettledAbsoluteCoords
						undet.fillAbsoluteCoordsMap()
						undet.x = absCoord[0] - relativeCoord[0]
						undet.y = absCoord[1] - relativeCoord[1]
						undet.z = absCoord[2] - relativeCoord[2]
						return undet, true
					}
				}
			}
		}
	}

	return undet, false
}

func makeAbsoluteCoordsList(absolute, relative [3]int, relativeCoords [][3]int) [][3]int {

	diff := [3]int{
		absolute[0] - relative[0],
		absolute[1] - relative[1],
		absolute[2] - relative[2],
	}

	var absCoords [][3]int
	for _, c := range relativeCoords {
		absCoords = append(absCoords, [3]int{
			diff[0] + c[0],
			diff[1] + c[1],
			diff[2] + c[2],
		})
	}

	return absCoords
}

func parseInput(input string) (ans []scanner) {
	for _, rawScanner := range strings.Split(input, "\n\n") {
		var number int
		lines := strings.Split(rawScanner, "\n")
		_, err := fmt.Sscanf(lines[0], "--- scanner %d ---", &number)
		if err != nil {
			panic("parsing error " + err.Error())
		}

		var coords [][3]int
		for _, line := range lines[1:] {
			var x, y, z int
			_, err := fmt.Sscanf(line, "%d,%d,%d", &x, &y, &z)
			if err != nil {
				panic("parsing error " + err.Error())
			}
			coords = append(coords, [3]int{x, y, z})
		}

		sc := scanner{
			number:            number,
			x:                 0,
			y:                 0,
			z:                 0,
			relativeCoords:    coords,
			absoluteCoords:    nil,
			absoluteCoordsMap: map[[3]int]bool{},
		}
		sc.fillRotations()
		ans = append(ans, sc)
	}

	return ans
}