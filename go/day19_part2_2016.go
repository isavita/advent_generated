package main

import (
	"fmt"
	"os"
	"path"
	"runtime"
	"strconv"
	"strings"
)

func main() {
	ans := elephant(readFile("./input.txt"))
	fmt.Println(ans)
}

type LLNode struct {
	elfNum   int
	presents int
	next     *LLNode
}

func elephant(input string) int {
	startingElves, _ := strconv.Atoi(input)
	root := &LLNode{
		elfNum:   1,
		presents: 1,
	}
	iter := root
	for i := 2; i <= startingElves; i++ {
		iter.next = &LLNode{
			elfNum:   i,
			presents: 1,
		}
		iter = iter.next
	}
	iter.next = root

	isOddLength := startingElves%2 == 1
	beforeAcross := root
	for i := 0; i < startingElves/2-1; i++ {
		beforeAcross = beforeAcross.next
	}

	for root.next != root {
		root.presents += beforeAcross.next.presents

		beforeAcross.next = beforeAcross.next.next

		if isOddLength {
			beforeAcross = beforeAcross.next
		}
		isOddLength = !isOddLength
		root = root.next
	}

	return root.elfNum
}

func readFile(pathFromCaller string) string {
	_, filename, _, ok := runtime.Caller(1)
	if !ok {
		panic("Not Found")
	}

	absolutePath := path.Join(path.Dir(filename), pathFromCaller)

	content, err := os.ReadFile(absolutePath)
	if err != nil {
		panic(err)
	}

	strContent := string(content)
	return strings.TrimRight(strContent, "\n")
}
