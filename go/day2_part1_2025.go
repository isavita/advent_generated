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

	// Remove any newlines or extra whitespace that might exist
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

	// Use a map to store unique invalid IDs found (to handle overlapping ranges)
	// We use string keys because big.Int cannot be map keys directly
	foundIDs := make(map[string]bool)

	// 2. Find Invalid IDs
	// An invalid ID is a number formed by a sequence repeated twice.
	// Mathematically, if the sequence is number S with length k, 
	// the ID is S * 10^k + S = S * (10^k + 1).
	// S must be a k-digit number: 10^(k-1) <= S <= 10^k - 1.

	// We check lengths 2, 4, 6 ... up to 18 (since int64/uint64 fits ~19 digits, 
	// and even big.Int inputs likely won't exceed practical limits for this puzzle style, 
	// but the code handles arbitrary size implicitly via big.Int logic if we wanted. 
	// For this specific logic, we'll loop standard lengths). 
	// Let's assume max ID length could be up to 20 digits to be safe.
	
	for _, r := range ranges {
		// Iterate possible half-lengths k. 
		// k=1 (ID len 2), k=2 (ID len 4), ... k=9 (ID len 18).
		for k := 1; k <= 10; k++ {
			// Multiplier M = 10^k + 1
			multiplier := new(big.Int).Exp(big.NewInt(10), big.NewInt(int64(k)), nil)
			multiplier.Add(multiplier, big.NewInt(1))

			// Min Seed (10^(k-1)) and Max Seed (10^k - 1)
			minSeed := new(big.Int).Exp(big.NewInt(10), big.NewInt(int64(k-1)), nil)
			maxSeed := new(big.Int).Exp(big.NewInt(10), big.NewInt(int64(k)), nil)
			maxSeed.Sub(maxSeed, big.NewInt(1))

			// Calculate the range of seeds S such that:
			// r.Start <= S * M <= r.End
			//
			// Lower bound for S: ceil(r.Start / M)
			// Upper bound for S: floor(r.End / M)

			sMin := new(big.Int).Set(r.Start)
			// For ceil(A/B), we do (A + B - 1) / B
			sMin.Add(sMin, multiplier)
			sMin.Sub(sMin, big.NewInt(1))
			sMin.Div(sMin, multiplier)

			sMax := new(big.Int).Set(r.End)
			sMax.Div(sMax, multiplier)

			// Intersect [sMin, sMax] with valid k-digit seeds [minSeed, maxSeed]
			start := sMin
			if start.Cmp(minSeed) < 0 {
				start = minSeed
			}

			end := sMax
			if end.Cmp(maxSeed) > 0 {
				end = maxSeed
			}

			// Iterate valid seeds
			// Using standard loop since bounds are found
			curr := new(big.Int).Set(start)
			for curr.Cmp(end) <= 0 {
				// Reconstruct ID: curr * M
				id := new(big.Int).Mul(curr, multiplier)
				foundIDs[id.String()] = true
				
				curr.Add(curr, big.NewInt(1))
			}
		}
	}

	// 3. Sum the unique IDs
	totalSum := new(big.Int)
	for idStr := range foundIDs {
		val, _ := new(big.Int).SetString(idStr, 10)
		totalSum.Add(totalSum, val)
	}

	fmt.Printf("Sum of invalid IDs: %s\n", totalSum.String())
}
