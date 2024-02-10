package main

import (
	"fmt"
	"io/os"
	"path"
	"runtime"
	"sort"
	"strings"
)

func main() {
	ans := rtgHellDay(readFile("./input.txt"), 1)
	fmt.Println(ans)
}

func rtgHellDay(input string, part int) int {
	currentState := newInitialState(input)

	if part == 2 {
		currentState.floors[0] = append(currentState.floors[0],
			halves{isChip: false, material: "elerium"},
			halves{isChip: true, material: "elerium"},
			halves{isChip: false, material: "dilithium"},
			halves{isChip: true, material: "dilithium"},
		)
	}

	queue := []state{currentState}
	prevStates := map[string]bool{}
	for len(queue) > 0 {
		front := queue[0]
		queue = queue[1:]

		if front.isDone() {
			return front.steps
		}

		hash := front.hashKey()
		if prevStates[hash] {
			continue
		}
		prevStates[hash] = true

		nextStates := front.getNextStates()
		queue = append(queue, nextStates...)
	}

	return -1
}

type halves struct {
	isChip   bool
	material string
}

func (t halves) String() string {
	tType := " generator"
	if t.isChip {
		tType = " microchip"
	}
	return fmt.Sprint(t.material, tType)
}

type state struct {
	floors        [4][]halves
	elevatorLevel int
	steps         int
}

func newInitialState(input string) state {
	s := state{}

	for lineIndex, line := range strings.Split(input, "\n") {

		parts := strings.Split(line, " ")

		for i, v := range parts {
			parts[i] = strings.Trim(v, ",.")
		}

		for i, word := range parts {
			if word == "generator" {
				material := parts[i-1]
				s.floors[lineIndex] = append(s.floors[lineIndex], halves{
					isChip:   false,
					material: material,
				})
			} else if word == "microchip" {

				material := parts[i-1][:strings.Index(parts[i-1], "-comp")]
				s.floors[lineIndex] = append(s.floors[lineIndex], halves{
					isChip:   true,
					material: material,
				})
			}
		}
	}

	return s
}

func (s state) String() string {
	var sb strings.Builder
	fmt.Fprintf(&sb, "Level %d x Steps %d\n", s.elevatorLevel, s.steps)
	for i, f := range s.floors {
		fmt.Fprintf(&sb, "  %d: %v\n", i, f)
	}
	return sb.String()
}

func (s state) hashKey() string {

	mapGenToIndex := map[string]int{}
	mapChipToIndex := map[string]int{}
	for flIndex, fl := range s.floors {
		for _, half := range fl {
			if half.isChip {
				mapChipToIndex[half.material] = flIndex
			} else {
				mapGenToIndex[half.material] = flIndex
			}
		}
	}

	var genChipPairs [][2]int
	for material := range mapGenToIndex {
		genChipPairs = append(genChipPairs, [2]int{
			mapGenToIndex[material], mapChipToIndex[material],
		})
	}

	sort.Slice(genChipPairs, func(i, j int) bool {
		if genChipPairs[i][0] != genChipPairs[j][0] {
			return genChipPairs[i][0] < genChipPairs[j][0]
		}
		return genChipPairs[i][1] < genChipPairs[j][1]
	})

	return fmt.Sprint(s.elevatorLevel, genChipPairs)
}

func (s state) isValid() bool {

	for i := range s.floors {

		gensSeen := map[string]bool{}
		for _, half := range s.floors[i] {
			if !half.isChip {
				gensSeen[half.material] = true
			}
		}

		if len(gensSeen) == 0 {
			continue
		}

		for _, half := range s.floors[i] {
			if half.isChip && !gensSeen[half.material] {
				return false
			}
		}
	}

	return true
}

func (s state) isDone() bool {
	var lenSum int
	for _, fl := range s.floors[:3] {
		lenSum += len(fl)
	}
	return lenSum == 0
}

func (s state) getMovablePermIndices() [][]int {
	var permsToMove [][]int

	currentLevel := s.floors[s.elevatorLevel]

	for i := 0; i < len(currentLevel); i++ {
		for j := i + 1; j < len(currentLevel); j++ {
			permsToMove = append(permsToMove, []int{i, j})
		}
	}

	for i := range currentLevel {
		permsToMove = append(permsToMove, []int{i})
	}
	return permsToMove
}

func (s state) clone() state {
	cl := state{
		elevatorLevel: s.elevatorLevel,
		steps:         s.steps,
	}

	for i, fl := range s.floors {
		cl.floors[i] = append([]halves{}, fl...)
	}
	return cl
}

func (s state) getNextStates() []state {
	var futureStates []state

	movablePermIndices := s.getMovablePermIndices()

	var eleDiffs []int
	if s.elevatorLevel < len(s.floors)-1 {
		eleDiffs = append(eleDiffs, 1)
	}
	if s.elevatorLevel > 0 {
		eleDiffs = append(eleDiffs, -1)
	}

	for _, eleDiff := range eleDiffs {

		for _, permIndices := range movablePermIndices {
			cl := s.clone()
			cl.elevatorLevel += eleDiff
			cl.steps++
			oldLevel := s.elevatorLevel
			newLevel := cl.elevatorLevel

			for _, index := range permIndices {
				cl.floors[newLevel] = append(cl.floors[newLevel], cl.floors[oldLevel][index])
			}

			for in := len(permIndices) - 1; in >= 0; in-- {
				cl.floors[oldLevel][permIndices[in]] = cl.floors[oldLevel][len(cl.floors[oldLevel])-1]
				cl.floors[oldLevel] = cl.floors[oldLevel][:len(cl.floors[oldLevel])-1]
			}

			if cl.isValid() {
				futureStates = append(futureStates, cl)
			}
		}
	}

	return futureStates
}

func readFile(pathFromCaller string) string {

	_, filename, _, ok := runtime.Caller(1)
	if !ok {
		panic("Not Found")
	}

	absolutePath := path.Join(path.Dir(filename), pathFromCaller)

	content, err := os.ReadFile(absolutePath)
	if err != nil {
		panic(err)
	}

	strContent := string(content)
	return strings.TrimRight(strContent, "\n")
}