package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	earliestDeparture, _ := strconv.Atoi(scanner.Text())
	scanner.Scan()
	busIDs := strings.Split(scanner.Text(), ",")

	earliestBusID := 0
	minWaitTime := earliestDeparture

	for _, id := range busIDs {
		if id == "x" {
			continue
		}
		busID, _ := strconv.Atoi(id)
		waitTime := busID - (earliestDeparture % busID)
		if waitTime < minWaitTime {
			minWaitTime = waitTime
			earliestBusID = busID
		}
	}

	fmt.Println(earliestBusID * minWaitTime)
}