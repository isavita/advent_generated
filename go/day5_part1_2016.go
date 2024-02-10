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
	var password strings.Builder
	for i := 0; password.Len() < 8; i++ {
		hash := md5Hash(doorID + strconv.Itoa(i))
		if strings.HasPrefix(hash, "00000") {
			password.WriteByte(hash[5])
		}
	}
	return password.String()
}

func md5Hash(input string) string {
	h := md5.New()
	io.WriteString(h, input)
	return fmt.Sprintf("%x", h.Sum(nil))
}