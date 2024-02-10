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
	res := auntSue(string(file))
	fmt.Println(res)
}

var targetSue = map[string]int{
	"children":    3,
	"cats":        7,
	"samoyeds":    2,
	"pomeranians": 3,
	"akitas":      0,
	"vizslas":     0,
	"goldfish":    5,
	"trees":       3,
	"cars":        2,
	"perfumes":    1,
}

func auntSue(input string) int {
	for _, line := range strings.Split(input, "\n") {
		var thing1, thing2, thing3 string
		var sueNum, amount1, amount2, amount3 int
		// Sue 1: goldfish: 6, trees: 9, akitas: 0
		_, err := fmt.Sscanf(line, "Sue %d: %s %d, %s %d, %s %d",
			&sueNum, &thing1, &amount1, &thing2, &amount2, &thing3, &amount3)
		if err != nil {
			panic(err)
		}
		thing1 = strings.Trim(thing1, ":")
		thing2 = strings.Trim(thing2, ":")
		thing3 = strings.Trim(thing3, ":")

		// put it in a map for part 2 to make it easy to look up a particular
		// thing's count
		readingsMap := map[string]int{}
		readingsMap[thing1] = amount1
		readingsMap[thing2] = amount2
		readingsMap[thing3] = amount3

		allRulesMatched := true
		// check ranges where the scanned number is LESS than target's
		for _, check := range []string{"cats", "trees"} {
			if scanCount, found := readingsMap[check]; found {
				if scanCount <= targetSue[check] {
					allRulesMatched = false
				}
				delete(readingsMap, check)
			}
		}
		// check ranges where chaned number is MORE than target's
		for _, check := range []string{"pomeranians", "goldfish"} {
			if scanCount, found := readingsMap[check]; found {
				if scanCount >= targetSue[check] {
					allRulesMatched = false
				}
				delete(readingsMap, check)
			}
		}

		// check literal amounts
		for thing, amount := range readingsMap {
			if targetSue[thing] != amount {
				allRulesMatched = false
			}
		}
		if allRulesMatched {
			return sueNum
		}
	}

	panic("expect return from loop")
}