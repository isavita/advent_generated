package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func generateAddresses(mask string, address int64) []int64 {
	floating := []int{}
	var addresses []int64

	// Apply mask '1's and collect floating positions
	for i, bit := range mask {
		if bit == '1' {
			address |= (1 << (35 - i))
		} else if bit == 'X' {
			floating = append(floating, 35-i)
		}
	}

	// Generate all combinations for the floating bits
	count := 1 << len(floating)
	for i := 0; i < count; i++ {
		modAddress := address
		for j, pos := range floating {
			if i&(1<<j) == 0 {
				modAddress &^= (1 << pos) // Clear bit
			} else {
				modAddress |= (1 << pos) // Set bit
			}
		}
		addresses = append(addresses, modAddress)
	}
	return addresses
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
				addresses := generateAddresses(mask, address)
				for _, addr := range addresses {
					mem[addr] = value
				}
			}
		}
	}

	var sum int64 = 0
	for _, value := range mem {
		sum += value
	}

	fmt.Printf("%d\n", sum)
}