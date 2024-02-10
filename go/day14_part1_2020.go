package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func applyMask(value int64, mask string) int64 {
	var result int64 = 0
	for i := 0; i < 36; i++ {
		bitValue := int64(1 << (35 - i))
		if mask[i] == '1' {
			result |= bitValue
		} else if mask[i] == 'X' {
			result |= (value & bitValue)
		}
	}
	return result
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	mask := ""
	mem := make(map[int64]int64)
	reMem := regexp.MustCompile(`mem\[(\d+)] = (\d+)`)

	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "mask = ") {
			mask = strings.TrimPrefix(line, "mask = ")
		} else {
			matches := reMem.FindStringSubmatch(line)
			if matches != nil {
				address, _ := strconv.ParseInt(matches[1], 10, 64)
				value, _ := strconv.ParseInt(matches[2], 10, 64)
				mem[address] = applyMask(value, mask)
			}
		}
	}

	var sum int64 = 0
	for _, value := range mem {
		sum += value
	}

	fmt.Printf("%d\n", sum)
}