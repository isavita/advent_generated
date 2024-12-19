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

	// Skip blank line
	sc.Scan()

	count := 0
	for sc.Scan() {
		design := sc.Text()
		if canMake(design, availablePatterns) {
			count++
		}
	}
	fmt.Println(count)
}

func canMake(design string, patterns []string) bool {
	n := len(design)
	dp := make([]bool, n+1)
	dp[0] = true
	for i := 1; i <= n; i++ {
		for _, p := range patterns {
			lp := len(p)
			if i >= lp && dp[i-lp] && design[i-lp:i] == p {
				dp[i] = true
				break
			}
		}
	}
	return dp[n]
}
