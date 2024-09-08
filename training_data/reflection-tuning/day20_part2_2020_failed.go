package main

import (
	"fmt"
	"math"
	"strings"
)

type Tile struct {
	ID    int
	Data  []string
	Edges [8]string // All possible edges (normal and flipped)
}

func parseTiles(input string) map[int]*Tile {
	tiles := make(map[int]*Tile)
	for _, tileStr := range strings.Split(input, "\n\n") {
		lines := strings.Split(tileStr, "\n")
		id := 0
		fmt.Sscanf(lines[0], "Tile %d:", &id)
		tile := &Tile{ID: id, Data: lines[1:]}
		tile.computeEdges()
		tiles[id] = tile
	}
	return tiles
}

func (t *Tile) computeEdges() {
	top := t.Data[0]
	bottom := t.Data[len(t.Data)-1]
	left := ""
	right := ""
	for _, row := range t.Data {
		left += string(row[0])
		right += string(row[len(row)-1])
	}
	t.Edges = [8]string{
		top, bottom, left, right,
		reverse(top), reverse(bottom), reverse(left), reverse(right),
	}
}

func reverse(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func findCorners(tiles map[int]*Tile) []int {
	corners := []int{}
	for id, tile := range tiles {
		matches := 0
		for _, edge := range tile.Edges[:4] {
			for otherID, otherTile := range tiles {
				if id != otherID {
					for _, otherEdge := range otherTile.Edges {
						if edge == otherEdge {
							matches++
							break
						}
					}
				}
			}
		}
		if matches == 2 {
			corners = append(corners, id)
		}
	}
	return corners
}

func assembleImage(tiles map[int]*Tile) []string {
	size := int(math.Sqrt(float64(len(tiles))))
	assembled := make([][]string, size)
	for i := range assembled {
		assembled[i] = make([]string, size)
	}

	// Implement backtracking algorithm to assemble the image
	// ...

	return flattenImage(assembled)
}

func flattenImage(assembled [][]string) []string {
	// Remove borders and flatten the image
	// ...
	return []string{} // Placeholder
}

func countSeaMonsters(image []string) int {
	monster := []string{
		"                  # ",
		"#    ##    ##    ###",
		" #  #  #  #  #  #   ",
	}
	// Implement sea monster detection
	// ...
	return 0 // Placeholder
}

func countHashes(image []string) int {
	count := 0
	for _, row := range image {
		count += strings.Count(row, "#")
	}
	return count
}

func solve(input string) (int, int) {
	tiles := parseTiles(input)
	corners := findCorners(tiles)

	part1 := 1
	for _, id := range corners {
		part1 *= id
	}

	image := assembleImage(tiles)
	monsters := countSeaMonsters(image)
	part2 := countHashes(image) - monsters*15 // 15 is the number of # in a sea monster

	return part1, part2
}

func main() {
	input := "" // Add your puzzle input here
	part1, part2 := solve(input)
	fmt.Printf("Part 1: %d\n", part1)
	fmt.Printf("Part 2: %d\n", part2)
}
