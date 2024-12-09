package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	data, _ := ioutil.ReadFile("input.txt")
	line := strings.TrimSpace(string(data))

	var disk []rune
	fileID := 0
	isFile := true
	for i := 0; i < len(line); i++ {
		length, _ := strconv.Atoi(string(line[i]))
		if isFile {
			for j := 0; j < length; j++ {
				disk = append(disk, rune('0'+fileID))
			}
			fileID++
		} else {
			for j := 0; j < length; j++ {
				disk = append(disk, '.')
			}
		}
		isFile = !isFile
	}

	for {
		lfree := -1
		for i := 0; i < len(disk); i++ {
			if disk[i] == '.' {
				lfree = i
				break
			}
		}
		if lfree == -1 {
			break
		}
		rfile := -1
		for i := len(disk) - 1; i > lfree; i-- {
			if disk[i] != '.' {
				rfile = i
				break
			}
		}
		if rfile == -1 {
			break
		}
		disk[lfree] = disk[rfile]
		disk[rfile] = '.'
	}

	var checksum int
	for i, b := range disk {
		if b != '.' {
			id := int(b - '0')
			checksum += i * id
		}
	}
	fmt.Println(checksum)
}
