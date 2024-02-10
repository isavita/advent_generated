package main

import (
	"crypto/md5"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

func main() {
	data, _ := os.ReadFile("input.txt")
	salt := strings.TrimSpace(string(data))
	keys := 0
	index := 0
	for ; keys < 64; index++ {
		hash := getMD5Hash(salt + strconv.Itoa(index))
		if triplet := findTriplet(hash); triplet != "" {
			for i := 1; i <= 1000; i++ {
				nextHash := getMD5Hash(salt + strconv.Itoa(index+i))
				if strings.Contains(nextHash, strings.Repeat(triplet, 5)) {
					keys++
					break
				}
			}
		}
	}
	fmt.Println(index - 1)
}

func getMD5Hash(input string) string {
	h := md5.New()
	io.WriteString(h, input)
	return fmt.Sprintf("%x", h.Sum(nil))
}

func findTriplet(hash string) string {
	for i := 0; i < len(hash)-2; i++ {
		if hash[i] == hash[i+1] && hash[i] == hash[i+2] {
			return string(hash[i])
		}
	}
	return ""
}