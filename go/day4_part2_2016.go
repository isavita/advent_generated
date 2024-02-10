package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if isRealRoom(line) {
			decryptedName := decryptName(line)
			if strings.Contains(decryptedName, "northpole object") {
				fmt.Println(getSectorID(line))
				break
			}
		}
	}
}

func isRealRoom(room string) bool {
	parts := strings.Split(room, "[")
	checksum := strings.TrimRight(parts[1], "]")
	encryptedName := strings.Split(parts[0], "-")
	encryptedName = encryptedName[:len(encryptedName)-1]

	letterCounts := make(map[rune]int)
	for _, part := range encryptedName {
		for _, letter := range part {
			letterCounts[letter]++
		}
	}

	type letterCount struct {
		letter rune
		count  int
	}

	var counts []letterCount
	for letter, count := range letterCounts {
		counts = append(counts, letterCount{letter, count})
	}

	sort.Slice(counts, func(i, j int) bool {
		if counts[i].count == counts[j].count {
			return counts[i].letter < counts[j].letter
		}
		return counts[i].count > counts[j].count
	})

	for i := 0; i < len(checksum); i++ {
		if checksum[i] != byte(counts[i].letter) {
			return false
		}
	}

	return true
}

func getSectorID(room string) int {
	parts := strings.Split(room, "-")
	sectorIDPart := parts[len(parts)-1]
	sectorID, _ := strconv.Atoi(strings.Split(sectorIDPart, "[")[0])
	return sectorID
}

func decryptName(room string) string {
	parts := strings.Split(room, "-")
	sectorIDPart := parts[len(parts)-1]
	sectorID, _ := strconv.Atoi(strings.Split(sectorIDPart, "[")[0])
	var decryptedName strings.Builder

	for _, part := range parts[:len(parts)-1] {
		for _, letter := range part {
			if letter == '-' {
				decryptedName.WriteRune(' ')
			} else {
				shiftedLetter := 'a' + ((int(letter-'a') + sectorID) % 26)
				decryptedName.WriteRune(rune(shiftedLetter))
			}
		}
		decryptedName.WriteRune(' ')
	}

	return strings.TrimSpace(decryptedName.String())
}