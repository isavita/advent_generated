package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type fileSegment struct {
	id    int
	start int
	end   int
}

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

	files := []fileSegment{}
	{
		var curID rune = -1
		var start int
		for i, b := range disk {
			if b == '.' {
				curID = -1
				continue
			}
			id := b - '0'
			if id != curID {
				curID = id
				start = i
			}
			if i == len(disk)-1 || (i+1 < len(disk) && disk[i+1]-'0' != id) {
				files = append(files, fileSegment{int(id), start, i})
			}
		}
	}

	for i := len(files) - 1; i >= 0; i-- {
		f := files[i]
		fileLen := f.end - f.start + 1
		leftmostSpan := -1
		spanLen := 0
		for j := 0; j < f.start; j++ {
			if disk[j] == '.' {
				if spanLen == 0 {
					leftmostSpan = j
				}
				spanLen++
				if spanLen == fileLen {
					break
				}
			} else {
				spanLen = 0
				leftmostSpan = -1
			}
		}
		if leftmostSpan != -1 && spanLen == fileLen {
			for x := f.start; x <= f.end; x++ {
				disk[x] = '.'
			}
			for x := 0; x < fileLen; x++ {
				disk[leftmostSpan+x] = rune('0' + f.id)
			}
		}
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
