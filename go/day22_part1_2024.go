package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func nextSecret(s uint64) uint64 {
	x := s * 64
	s ^= x
	s &= 0xFFFFFF
	x = s / 32
	s ^= x
	s &= 0xFFFFFF
	x = s * 2048
	s ^= x
	s &= 0xFFFFFF
	return s
}

func main() {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	var buyers []uint64
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		n, err := strconv.ParseUint(line, 10, 64)
		if err != nil {
			log.Fatal(err)
		}
		buyers = append(buyers, n)
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	var total uint64
	for _, b := range buyers {
		s := b
		for i := 0; i < 2000; i++ {
			s = nextSecret(s)
		}
		total += s
	}

	fmt.Println(total)
}
