package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	data, _ := ioutil.ReadFile("input.txt")
	line := strings.TrimSpace(string(data))
	stonesStr := strings.Fields(line)
	stones := make([]string, len(stonesStr))
	copy(stones, stonesStr)

	for i := 0; i < 25; i++ {
		next := make([]string, 0, len(stones)*2)
		for _, s := range stones {
			if s == "0" {
				next = append(next, "1")
			} else if evenDigits(s) {
				mid := len(s)/2
				left := trimLeadingZeros(s[:mid])
				right := trimLeadingZeros(s[mid:])
				if left == "" { left = "0" }
				if right == "" { right = "0" }
				next = append(next, left, right)
			} else {
				n, _ := strconv.Atoi(s)
				next = append(next, strconv.Itoa(n*2024))
			}
		}
		stones = next
	}

	fmt.Println(len(stones))
}

func evenDigits(s string) bool {
	return len(s)%2 == 0
}

func trimLeadingZeros(s string) string {
	for len(s) > 1 && s[0] == '0' {
		s = s[1:]
	}
	return s
}
