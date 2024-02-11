package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var vals []int
	for scanner.Scan() {
		text := scanner.Text()
		if text == "" {
			continue
		}
		val, err := strconv.Atoi(text)
		if err != nil {
			panic(err)
		}
		vals = append(vals, val)
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	prevSum := vals[0] + vals[1] + vals[2]
	count := 0
	for i := 3; i < len(vals); i++ {
	    currSum := vals[i-2] + vals[i-1] + vals[i]
	    if currSum > prevSum {
		count++
	    }
	    prevSum = currSum
	}

	fmt.Println(count)
}
