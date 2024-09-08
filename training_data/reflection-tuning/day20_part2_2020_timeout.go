package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Tile struct {
	ID    int
	Data  []string
	Edges [8]string // 4 original edges + 4 flipped edges
}

func main() {
	tiles := readInput("input.txt")
	assembled := assembleTiles(tiles)
	
	// Part 1
	corners := findCorners(assembled)
	result := 1
	for _, id := range corners {
		result *= id
	}
	fmt.Println("Part 1:", result)

	// Part 2
	image := createFinalImage(assembled)
	roughness := countRoughness(image)
	fmt.Println("Part 2:", roughness)
}

func readInput(filename string) map[int]Tile {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)

	tiles := make(map[int]Tile)
	var currentTile Tile
	var data []string

	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "Tile") {
			if currentTile.ID != 0 {
				currentTile.Data = data
				calculateEdges(&currentTile)
				tiles[currentTile.ID] = currentTile
			}
			id, _ := strconv.Atoi(strings.Trim(line[5:], ":"))
			currentTile = Tile{ID: id}
			data = []string{}
		} else if line != "" {
			data = append(data, line)
		}
	}

	currentTile.Data = data
	calculateEdges(&currentTile)
	tiles[currentTile.ID] = currentTile

	return tiles
}

func calculateEdges(tile *Tile) {
	top := tile.Data[0]
	bottom := tile.Data[len(tile.Data)-1]
	left := ""
	right := ""
	for _, row := range tile.Data {
		left += string(row[0])
		right += string(row[len(row)-1])
	}

	tile.Edges = [8]string{
		top, right, bottom, left,
		reverse(top), reverse(right), reverse(bottom), reverse(left),
	}
}

func reverse(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func assembleTiles(tiles map[int]Tile) [][]Tile {
	size := int(sqrt(float64(len(tiles))))
	assembled := make([][]Tile, size)
	for i := range assembled {
		assembled[i] = make([]Tile, size)
	}

	used := make(map[int]bool)
	backtrack(tiles, assembled, 0, 0, used)
	return assembled
}

func backtrack(tiles map[int]Tile, assembled [][]Tile, row, col int, used map[int]bool) bool {
	if row == len(assembled) {
		return true
	}

	nextRow, nextCol := row, col+1
	if nextCol == len(assembled) {
		nextRow, nextCol = row+1, 0
	}

	for id, tile := range tiles {
		if used[id] {
			continue
		}

		for i := 0; i < 8; i++ {
			rotatedTile := rotateTile(tile, i)
			if isValidPlacement(assembled, rotatedTile, row, col) {
				assembled[row][col] = rotatedTile
				used[id] = true

				if backtrack(tiles, assembled, nextRow, nextCol, used) {
					return true
				}

				delete(used, id)
			}
		}
	}

	return false
}

func isValidPlacement(assembled [][]Tile, tile Tile, row, col int) bool {
	if row > 0 {
		if assembled[row-1][col].Edges[2] != tile.Edges[0] {
			return false
		}
	}
	if col > 0 {
		if assembled[row][col-1].Edges[1] != tile.Edges[3] {
			return false
		}
	}
	return true
}

func rotateTile(tile Tile, rotation int) Tile {
	newTile := Tile{ID: tile.ID}
	newTile.Edges = [8]string{}
	copy(newTile.Edges[:], tile.Edges[rotation:])
	copy(newTile.Edges[8-rotation:], tile.Edges[:rotation])

	newTile.Data = make([]string, len(tile.Data))
	for i := range newTile.Data {
		newTile.Data[i] = tile.Data[i]
	}

	// Apply rotations and flips
	for i := 0; i < rotation%4; i++ {
		newTile.Data = rotateTileData(newTile.Data)
	}
	if rotation >= 4 {
		newTile.Data = flipTileData(newTile.Data)
	}

	return newTile
}

func rotateTileData(data []string) []string {
	n := len(data)
	rotated := make([]string, n)
	for i := 0; i < n; i++ {
		for j := n - 1; j >= 0; j-- {
			rotated[i] += string(data[j][i])
		}
	}
	return rotated
}

func flipTileData(data []string) []string {
	flipped := make([]string, len(data))
	for i, row := range data {
		flipped[i] = reverse(row)
	}
	return flipped
}

func findCorners(assembled [][]Tile) []int {
	corners := []int{
		assembled[0][0].ID,
		assembled[0][len(assembled)-1].ID,
		assembled[len(assembled)-1][0].ID,
		assembled[len(assembled)-1][len(assembled)-1].ID,
	}
	return corners
}

func createFinalImage(assembled [][]Tile) []string {
	tileSize := len(assembled[0][0].Data) - 2
	imageSize := len(assembled) * tileSize
	image := make([]string, imageSize)

	for i := range assembled {
		for j := range assembled[i] {
			for k := 1; k < len(assembled[i][j].Data)-1; k++ {
				row := assembled[i][j].Data[k][1 : len(assembled[i][j].Data[k])-1]
				image[i*tileSize+k-1] += row
			}
		}
	}

	return image
}

func countRoughness(image []string) int {
	monster := []string{
		"                  # ",
		"#    ##    ##    ###",
		" #  #  #  #  #  #   ",
	}

	monsterCount := 0
	for i := 0; i < 8; i++ {
		rotatedImage := rotateTileData(image)
		if i == 4 {
			rotatedImage = flipTileData(rotatedImage)
		}

		monsterCount = findSeaMonsters(rotatedImage, monster)
		if monsterCount > 0 {
			break
		}
	}

	totalHash := 0
	for _, row := range image {
		totalHash += strings.Count(row, "#")
	}

	monsterHash := 0
	for _, row := range monster {
		monsterHash += strings.Count(row, "#")
	}

	return totalHash - monsterCount*monsterHash
}

func findSeaMonsters(image []string, monster []string) int {
	count := 0
	for i := 0; i <= len(image)-len(monster); i++ {
		for j := 0; j <= len(image[0])-len(monster[0]); j++ {
			if isMonsterAt(image, monster, i, j) {
				count++
			}
		}
	}
	return count
}

func isMonsterAt(image []string, monster []string, row, col int) bool {
	for i := 0; i < len(monster); i++ {
		for j := 0; j < len(monster[0]); j++ {
			if monster[i][j] == '#' && image[row+i][col+j] != '#' {
				return false
			}
		}
	}
	return true
}

func sqrt(n float64) float64 {
	return n * n
}
