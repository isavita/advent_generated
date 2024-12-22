package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

const (
	mod       = 1 << 24
	numSteps  = 2000
)

// nextSecret evolves the secret number by one step.
func nextSecret(s uint64) uint64 {
	x := s * 64
	s ^= x
	s &= mod - 1
	x = s / 32
	s ^= x
	s &= mod - 1
	x = s * 2048
	s ^= x
	s &= mod - 1
	return s
}

// encodeChange4 turns a 4-tuple of changes c1..c4 (each in -9..9)
// into a single integer so we can store sums in a slice instead of a map.
func encodeChange4(c1, c2, c3, c4 int) int {
	// Shift each change by +9 so they map to [0..18].
	// Then each tuple maps to an integer in [0..19^4-1].
	return (c1+9) + (c2+9)*19 + (c3+9)*19*19 + (c4+9)*19*19*19
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	var initials []uint64
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		n, err := strconv.ParseUint(line, 10, 64)
		if err != nil {
			log.Fatal(err)
		}
		initials = append(initials, n)
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	// For each buyer, we'll generate 2001 secret numbers (the initial + 2000 new).
	// Then we take the last digit of each secret number as the price.
	// We'll have 2001 prices and 2000 changes for each buyer.
	type buyerData struct {
		prices  []int
		changes []int
	}

	buyers := make([]buyerData, len(initials))
	for i, initVal := range initials {
		prices := make([]int, numSteps+1)
		s := initVal
		for j := 0; j <= numSteps; j++ {
			prices[j] = int(s % 10)
			if j < numSteps {
				s = nextSecret(s)
			}
		}
		changes := make([]int, numSteps)
		for j := 0; j < numSteps; j++ {
			changes[j] = prices[j+1] - prices[j]
		}
		buyers[i] = buyerData{prices, changes}
	}

	// We want a single 4-change pattern that, for each buyer, yields the earliest sell price
	// (i.e. the price at the time the pattern completes).
	// We'll track the earliest occurrence for each 4-change pattern per buyer.
	// Then we'll accumulate sums across all buyers.
	//
	// Approach:
	// 1) For each buyer, build a local array earliestPrice[19^4], initialized to -1 (meaning pattern not found).
	// 2) Slide through the buyer's 2000 changes. For each i in [0..1996], we form (c[i], c[i+1], c[i+2], c[i+3]).
	//    If earliestPrice[...] is -1, set earliestPrice[...] = prices[i+4] (the price at that step).
	// 3) After processing the buyer, we add earliestPrice[...] to a global sum array for each pattern that was found.
	//
	// Finally, the answer is the maximum entry in that global sum array.

	const patternCount = 19 * 19 * 19 * 19
	globalSum := make([]int64, patternCount)

	for _, b := range buyers {
		localPrice := make([]int, patternCount)
		for i := 0; i < patternCount; i++ {
			localPrice[i] = -1
		}
		for i := 0; i+3 < numSteps; i++ {
			c1, c2, c3, c4 := b.changes[i], b.changes[i+1], b.changes[i+2], b.changes[i+3]
			if c1 < -9 || c1 > 9 || c2 < -9 || c2 > 9 ||
			   c3 < -9 || c3 > 9 || c4 < -9 || c4 > 9 {
				continue
			}
			idx := encodeChange4(c1, c2, c3, c4)
			if localPrice[idx] < 0 {
				localPrice[idx] = b.prices[i+4]
			}
		}
		for idx, p := range localPrice {
			if p >= 0 {
				globalSum[idx] += int64(p)
			}
		}
	}

	var ans int64
	for _, s := range globalSum {
		if s > ans {
			ans = s
		}
	}

	fmt.Println(ans)
}
