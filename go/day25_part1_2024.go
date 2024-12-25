package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer f.Close()
	sc := bufio.NewScanner(f)

	var raw []string
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line != "" {
			raw = append(raw, line)
		}
	}
	if len(raw)%7 != 0 {
		fmt.Println("0")
		return
	}

	var locks, keys [][]int
	for i := 0; i+7 <= len(raw); i += 7 {
		block := raw[i : i+7]
		// Verify each line is at least 5 characters
		valid := true
		for _, ln := range block {
			if len(ln) < 5 {
				valid = false
				break
			}
		}
		if !valid {
			continue
		}
		// Distinguish lock vs key by looking at the first row
		// (all '#' => lock, all '.' => key)
		if allChar(block[0], '#') {
			locks = append(locks, parseLock(block))
		} else {
			keys = append(keys, parseKey(block))
		}
	}

	count := 0
	for _, lock := range locks {
		for _, key := range keys {
			if fits(lock, key) {
				count++
			}
		}
	}
	fmt.Println(count)
}

func parseLock(b []string) []int {
	h := make([]int, 5)
	for c := 0; c < 5; c++ {
		cnt := 0
		for r := 1; r < 7; r++ {
			if b[r][c] == '#' {
				cnt++
			} else {
				break
			}
		}
		h[c] = cnt
	}
	return h
}

func parseKey(b []string) []int {
	h := make([]int, 5)
	for c := 0; c < 5; c++ {
		cnt := 0
		for r := 5; r >= 0; r-- {
			if b[r][c] == '#' {
				cnt++
			} else {
				break
			}
		}
		h[c] = cnt
	}
	return h
}

func fits(lock, key []int) bool {
	for i := 0; i < 5; i++ {
		if lock[i]+key[i] > 5 {
			return false
		}
	}
	return true
}

func allChar(s string, ch rune) bool {
	for _, r := range s {
		if r != ch {
			return false
		}
	}
	return true
}
