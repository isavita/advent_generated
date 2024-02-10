package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

type node struct {
	used, avail int
}

func main() {
	nodes := readNodes("input.txt")
	viablePairs := countViablePairs(nodes)
	fmt.Println(viablePairs)
}

func readNodes(filename string) []node {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var nodes []node
	scanner := bufio.NewScanner(file)
	nodeRegex := regexp.MustCompile(`node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%`)
	for scanner.Scan() {
		if matches := nodeRegex.FindStringSubmatch(scanner.Text()); matches != nil {
			used, _ := strconv.Atoi(matches[1])
			avail, _ := strconv.Atoi(matches[2])
			nodes = append(nodes, node{used: used, avail: avail})
		}
	}
	return nodes
}

func countViablePairs(nodes []node) int {
	count := 0
	for i, a := range nodes {
		for j, b := range nodes {
			if i != j && a.used > 0 && a.used <= b.avail {
				count++
			}
		}
	}
	return count
}