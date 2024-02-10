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
	id, err := os.ReadFile("input.txt")
	doorID := strings.TrimSpace(string(id))
	if err != nil {
		panic(err)
	}
	password := findPassword(doorID)
	fmt.Println(password)
}

func findPassword(doorID string) string {
	password := make([]rune, 8)
	filledPositions := 0
	found := make([]bool, 8)

	for i := 0; filledPositions < 8; i++ {
		hash := md5Hash(doorID + strconv.Itoa(i))
		if strings.HasPrefix(hash, "00000") {
			pos := hash[5]
			if pos >= '0' && pos <= '7' {
				posIndex := int(pos - '0')
				if !found[posIndex] {
					found[posIndex] = true
					password[posIndex] = rune(hash[6])
					filledPositions++
				}
			}
		}
	}
	return string(password)
}

func md5Hash(input string) string {
	h := md5.New()
	io.WriteString(h, input)
	return fmt.Sprintf("%x", h.Sum(nil))
}