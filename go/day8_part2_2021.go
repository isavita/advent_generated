package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
	"sort"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))

	ans := jumbledSevenSegment(input)
	fmt.Println(ans)
}

func jumbledSevenSegment(input string) int {
	var parsedInput [][]string
	for i, line := range strings.Split(input, "\n") {

		parts := regexp.MustCompile(`([a-g]+)`).FindAllString(line, -1)

		if len(parts) != 14 {
			log.Fatalf("should be 14 parts in each input line, got %d for line %d", len(parts), i)
		}

		var fourteen []string
		for _, v := range parts {
			fourteen = append(fourteen, alphabetizeString(v))
		}
		parsedInput = append(parsedInput, fourteen)
	}

	var ans int
	indexToCharacters := make([]string, 10)
	for _, set := range parsedInput {

		workingSet := set[:10]
		var killIndices []int

		for i, mapping := range workingSet {
			switch len(mapping) {
			case 2:

				indexToCharacters[1] = mapping
				killIndices = append(killIndices, i)
			case 4:
				indexToCharacters[4] = mapping
				killIndices = append(killIndices, i)
			case 3:
				indexToCharacters[7] = mapping
				killIndices = append(killIndices, i)
			case 7:
				indexToCharacters[8] = mapping
				killIndices = append(killIndices, i)
			}
		}

		workingSet = removeSliceIndices(workingSet, killIndices...)

		var zeroThreeOrNine []string
		killIndices = []int{}
		for i, mapping := range workingSet {
			if checkStringOverlap(mapping, indexToCharacters[1]) {
				zeroThreeOrNine = append(zeroThreeOrNine, mapping)
				killIndices = append(killIndices, i)
			}
		}
		if len(zeroThreeOrNine) != 3 {
			log.Fatalf("one three or nine does not have three matches: got %d", len(zeroThreeOrNine))
		}

		for i, maybe039 := range zeroThreeOrNine {
			if len(maybe039) == 5 {

				indexToCharacters[3] = maybe039
				zeroThreeOrNine = removeSliceIndices(zeroThreeOrNine, i)
				break
			}
		}

		for i, maybe09 := range zeroThreeOrNine {
			if checkStringOverlap(maybe09, indexToCharacters[4]) {
				indexToCharacters[9] = maybe09
				zeroThreeOrNine = removeSliceIndices(zeroThreeOrNine, i)
			}
		}

		indexToCharacters[0] = zeroThreeOrNine[0]

		workingSet = removeSliceIndices(workingSet, killIndices...)
		if len(workingSet) != 3 {
			log.Fatalf("expected length of 3 at this stage, got %d", len(workingSet))
		}

		for i, mapping := range workingSet {
			if len(mapping) == 6 {
				indexToCharacters[6] = mapping
				workingSet = removeSliceIndices(workingSet, i)
			}
		}

		for i, mapping := range workingSet {
			if checkStringOverlap(indexToCharacters[9], mapping) {
				indexToCharacters[5] = mapping
				workingSet = removeSliceIndices(workingSet, i)
			}
		}

		if len(workingSet) != 1 {
			log.Fatalf("expected length of 1 at this stage, got %d", len(workingSet))
		}

		indexToCharacters[2] = workingSet[0]

		var num int
		for _, out := range set[10:] {
			for i, mapping := range indexToCharacters {

				if out == mapping {

					num *= 10
					num += i
				}
			}
		}
		ans += num
	}

	return ans
}

func removeSliceIndices(sli []string, indices ...int) []string {
	m := map[int]bool{}
	for _, v := range indices {
		m[v] = true
	}

	var ans []string
	for i, v := range sli {
		if !m[i] {
			ans = append(ans, v)
		}
	}
	return ans
}

func checkStringOverlap(larger, smaller string) bool {

	if len(larger) < len(smaller) {
		larger, smaller = smaller, larger
	}

	largeMap := map[rune]bool{}
	for _, r := range larger {
		largeMap[r] = true
	}

	for _, r := range smaller {
		if !largeMap[r] {
			return false
		}
	}
	return true
}

func alphabetizeString(input string) string {
	chars := strings.Split(input, "")
	sort.Strings(chars)
	return strings.Join(chars, "")
}