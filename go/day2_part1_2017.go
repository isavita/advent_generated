package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}

	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	checksum := 0

	for _, line := range lines {
		nums := strings.Fields(line)
		minVal, _ := strconv.Atoi(nums[0])
		maxVal, _ := strconv.Atoi(nums[0])

		for _, numStr := range nums {
			num, _ := strconv.Atoi(numStr)
			if num < minVal {
				minVal = num
			}
			if num > maxVal {
				maxVal = num
			}
		}

		checksum += (maxVal - minVal)
	}

	fmt.Println(checksum)
}