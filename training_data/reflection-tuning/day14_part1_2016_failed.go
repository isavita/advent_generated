package main

import (
	"crypto/md5"
	"fmt"
	"strings"
)

const salt = "abc" // Replace with your actual salt

func generateHash(salt string, index int) string {
	data := []byte(fmt.Sprintf("%s%d", salt, index))
	return fmt.Sprintf("%x", md5.Sum(data))
}

func findRepeat(s string, n int) byte {
	for i := 0; i < len(s)-n+1; i++ {
		if strings.Count(s[i:i+n], string(s[i])) == n {
			return s[i]
		}
	}
	return 0
}

func isKey(index int) bool {
	hash := generateHash(salt, index)
	triplet := findRepeat(hash, 3)
	if triplet == 0 {
		return false
	}

	for i := 1; i <= 1000; i++ {
		nextHash := generateHash(salt, index+i)
		if strings.Contains(nextHash, strings.Repeat(string(triplet), 5)) {
			return true
		}
	}
	return false
}

func main() {
	keyCount := 0
	index := 0

	for keyCount < 64 {
		if isKey(index) {
			keyCount++
			if keyCount == 64 {
				fmt.Println(index)
				break
			}
		}
		index++
	}
}
