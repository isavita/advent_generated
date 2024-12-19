package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	f, _ := os.Open("input.txt")
	defer f.Close()
	sc := bufio.NewScanner(f)

	sc.Scan()
	availableLine := sc.Text()
	availablePatterns := strings.Split(strings.TrimSpace(availableLine), ",")
	for i := range availablePatterns {
		availablePatterns[i] = strings.TrimSpace(availablePatterns[i])
	}

	sc.Scan()
	totalWays := 0
	for sc.Scan() {
		design := sc.Text()
		ways := countWays(design, availablePatterns)
		totalWays += ways
	}
	fmt.Println(totalWays)
}

func countWays(design string, patterns []string) int {
	n := len(design)
	dp := make([]int, n+1)
	dp[0] = 1
	for i := 1; i <= n; i++ {
		for _, p := range patterns {
			lp := len(p)
			if i >= lp && design[i-lp:i] == p {
				dp[i] += dp[i-lp]
			}
		}
	}
	return dp[n]
}
