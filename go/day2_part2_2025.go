package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"math/big"
	"strings"
)

// Range represents an inclusive interval [Start, End]
type Range struct {
	Start *big.Int
	End   *big.Int
}

func main() {
	// 1. Read and Parse Input
	content, err := ioutil.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	// Clean input string
	data := strings.ReplaceAll(string(content), "\n", "")
	data = strings.ReplaceAll(data, "\r", "")
	data = strings.TrimSpace(data)

	// Split by comma to get ranges
	parts := strings.Split(data, ",")
	var ranges []Range

	for _, p := range parts {
		if p == "" {
			continue
		}
		bounds := strings.Split(p, "-")
		if len(bounds) != 2 {
			log.Fatalf("Invalid range format: %s", p)
		}

		start, ok1 := new(big.Int).SetString(bounds[0], 10)
		end, ok2 := new(big.Int).SetString(bounds[1], 10)

		if !ok1 || !ok2 {
			log.Fatalf("Invalid numbers in range: %s", p)
		}

		ranges = append(ranges, Range{Start: start, End: end})
	}

	// Map to store unique invalid IDs found (to handle overlaps or multiple patterns for same number)
	foundIDs := make(map[string]bool)

	// 2. Iterate through Ranges
	for _, r := range ranges {
		sLen := len(r.Start.String())
		eLen := len(r.End.String())

		// Check IDs with total number of digits from length of Start to length of End
		for totalLen := sLen; totalLen <= eLen; totalLen++ {
			
			// An ID is valid if it consists of a seed of length 'k' repeated 'reps' times.
			// totalLen must be divisible by k, and reps must be >= 2.
			// We iterate possible seed lengths 'k'.
			for k := 1; k <= totalLen/2; k++ {
				if totalLen%k != 0 {
					continue
				}
				reps := totalLen / k

				// Calculate Pattern Multiplier M
				// If seed is S, the ID is S * M.
				// M = 10^0 + 10^k + 10^2k + ... + 10^(reps-1)k
				M := new(big.Int)
				for i := 0; i < reps; i++ {
					// shift = i * k
					shift := big.NewInt(int64(i * k))
					// term = 10^shift
					term := new(big.Int).Exp(big.NewInt(10), shift, nil)
					M.Add(M, term)
				}

				// The seed S must be a k-digit number.
				// MinSeed = 10^(k-1)
				// MaxSeed = 10^k - 1
				minSeed := new(big.Int).Exp(big.NewInt(10), big.NewInt(int64(k-1)), nil)
				maxSeed := new(big.Int).Exp(big.NewInt(10), big.NewInt(int64(k)), nil)
				maxSeed.Sub(maxSeed, big.NewInt(1))

				// We need to find S such that:
				// r.Start <= S * M <= r.End
				//
				// Therefore:
				// S >= r.Start / M  =>  S_min = Ceil(r.Start / M)
				// S <= r.End / M    =>  S_max = Floor(r.End / M)

				// Calculate TargetMin = Ceil(r.Start / M)
				targetMin := new(big.Int).Add(r.Start, M)
				targetMin.Sub(targetMin, big.NewInt(1))
				targetMin.Div(targetMin, M)

				// Calculate TargetMax = Floor(r.End / M)
				targetMax := new(big.Int).Div(r.End, M)

				// Intersect the valid seed range [minSeed, maxSeed] with needed range [targetMin, targetMax]
				start := targetMin
				if start.Cmp(minSeed) < 0 {
					start = minSeed
				}

				end := targetMax
				if end.Cmp(maxSeed) > 0 {
					end = maxSeed
				}

				// Generate all IDs in the valid intersection
				if start.Cmp(end) <= 0 {
					curr := new(big.Int).Set(start)
					for curr.Cmp(end) <= 0 {
						// Reconstruct ID: curr * M
						id := new(big.Int).Mul(curr, M)
						foundIDs[id.String()] = true
						
						curr.Add(curr, big.NewInt(1))
					}
				}
			}
		}
	}

	// 3. Sum all unique invalid IDs
	totalSum := new(big.Int)
	for idStr := range foundIDs {
		val, _ := new(big.Int).SetString(idStr, 10)
		totalSum.Add(totalSum, val)
	}

	fmt.Printf("Sum of invalid IDs: %s\n", totalSum.String())
}
