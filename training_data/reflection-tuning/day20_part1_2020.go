package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Tile struct {
	ID      int
	Borders []string
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	tiles := make(map[int]*Tile)
	borderCounts := make(map[string]int)

	var currentTile *Tile
	var tileLines []string

	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "Tile") {
			if currentTile != nil {
				processTile(currentTile, tileLines, tiles, borderCounts)
			}
			id, _ := strconv.Atoi(strings.TrimSuffix(strings.TrimPrefix(line, "Tile "), ":"))
			currentTile = &Tile{ID: id}
			tileLines = []string{}
		} else if line != "" {
			tileLines = append(tileLines, line)
		}
	}
	if currentTile != nil {
		processTile(currentTile, tileLines, tiles, borderCounts)
	}

	result := 1
	for _, tile := range tiles {
		matchingBorders := 0
		for _, border := range tile.Borders {
			if borderCounts[border] > 1 || borderCounts[reverse(border)] > 1 {
				matchingBorders++
			}
		}
		if matchingBorders == 2 {
			result *= tile.ID
		}
	}

	fmt.Println(result)
}

func processTile(tile *Tile, lines []string, tiles map[int]*Tile, borderCounts map[string]int) {
	top := lines[0]
	bottom := lines[len(lines)-1]
	left := ""
	right := ""
	for _, line := range lines {
		left += string(line[0])
		right += string(line[len(line)-1])
	}
	tile.Borders = []string{top, right, bottom, left}
	tiles[tile.ID] = tile

	for _, border := range tile.Borders {
		borderCounts[border]++
		borderCounts[reverse(border)]++
	}
}

func reverse(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}
