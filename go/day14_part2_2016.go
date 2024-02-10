package main

import (
	"crypto/md5"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

var hashCache = make(map[string]string)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	salt := strings.TrimSpace(string(data))
	keys := 0
	index := 0
	for ; keys < 64; index++ {
		hash := getStretchedMD5Hash(salt + strconv.Itoa(index))
		if triplet := findTriplet(hash); triplet != "" {
			for i := 1; i <= 1000; i++ {
				nextHash := getStretchedMD5Hash(salt + strconv.Itoa(index+i))
				if strings.Contains(nextHash, strings.Repeat(triplet, 5)) {
					keys++
					break
				}
			}
		}
	}

	fmt.Println(index - 1)
}

func getStretchedMD5Hash(input string) string {
	if hash, found := hashCache[input]; found {
		return hash
	}
	hash := getMD5Hash(input)
	for i := 0; i < 2016; i++ {
		hash = getMD5Hash(hash)
	}
	hashCache[input] = hash
	return hash
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