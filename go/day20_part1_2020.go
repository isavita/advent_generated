package main

import (
	"fmt"
	"math"
	"os"
	"strings"
)

func main() {
	file, err := os.ReadFile("input.txt")
	if err != nil {
		panic(err)
	}
	input := strings.TrimSpace(string(file))
	ans := solve(input)
	fmt.Println(ans)
}

func solve(input string) int {
	tiles := parseTilesFromInput(input)
	edgeSize := int(math.Sqrt(float64(len(tiles))))

	assembledTiles := backtrackAssemble(tiles, nil, map[int]bool{})

	product := assembledTiles[0][0].id
	product *= assembledTiles[0][edgeSize-1].id
	product *= assembledTiles[edgeSize-1][0].id
	product *= assembledTiles[edgeSize-1][edgeSize-1].id
	return product
}

type tile struct {
	contents [][]string
	id       int
}

func parseTilesFromInput(input string) []*tile {
	ans := []*tile{}
	for _, block := range strings.Split(input, "\n\n") {
		split := strings.Split(block, "\n")
		var tileID int
		_, err := fmt.Sscanf(split[0], "Tile %d:", &tileID)
		if err != nil {
			panic(err)
		}

		var contents [][]string
		for _, line := range split[1:] {
			contents = append(contents, strings.Split(line, ""))
		}
		ans = append(ans, &tile{id: tileID, contents: contents})
	}
	return ans
}

func backtrackAssemble(tiles []*tile, assembledTiles [][]*tile, usedIndices map[int]bool) [][]*tile {

	edgeSize := int(math.Sqrt(float64(len(tiles))))
	if assembledTiles == nil {
		assembledTiles = make([][]*tile, edgeSize)
		for i := 0; i < edgeSize; i++ {
			assembledTiles[i] = make([]*tile, edgeSize)
		}
	}

	for row := 0; row < edgeSize; row++ {
		for col := 0; col < edgeSize; col++ {

			if assembledTiles[row][col] == nil {

				for i, t := range tiles {
					if !usedIndices[i] {

						for _, opt := range allGridOrientations(t.contents) {

							if row != 0 {
								currentTopRow := getRow(opt, true)
								bottomOfAbove := getRow(assembledTiles[row-1][col].contents, false)

								if currentTopRow != bottomOfAbove {
									continue
								}
							}
							if col != 0 {
								currentLeftCol := getCol(opt, true)
								rightColOfLeft := getCol(assembledTiles[row][col-1].contents, false)
								if currentLeftCol != rightColOfLeft {
									continue
								}
							}

							t.contents = opt
							assembledTiles[row][col] = t

							usedIndices[i] = true
							recurseResult := backtrackAssemble(tiles, assembledTiles, usedIndices)
							if recurseResult != nil {
								return recurseResult
							}

							assembledTiles[row][col] = nil
							usedIndices[i] = false
						}
					}
				}

				if assembledTiles[row][col] == nil {
					return nil
				}
			}
		}
	}

	return assembledTiles
}

func getCol(grid [][]string, firstCol bool) string {
	var str string
	for i := range grid {
		if firstCol {
			str += grid[i][0]
		} else {
			str += grid[i][len(grid[0])-1]
		}
	}
	return str
}

func getRow(grid [][]string, firstRow bool) string {
	var str string
	for i := range grid[0] {
		if firstRow {
			str += grid[0][i]
		} else {
			str += grid[len(grid)-1][i]
		}
	}
	return str
}

func removeBordersFromGrid(grid [][]string) [][]string {
	var result [][]string

	for i := 1; i < len(grid)-1; i++ {
		result = append(result, []string{})
		for j := 1; j < len(grid[0])-1; j++ {
			result[i-1] = append(result[i-1], grid[i][j])
		}
	}

	return result
}

var monster = `                  # 
#    ##    ##    ###
 #  #  #  #  #  #   `

func findMonsterCoords(image [][]string) [][2]int {
	var monsterOffsets [][2]int
	var monsterHeight, monsterLength int
	for r, line := range strings.Split(monster, "\n") {
		for c, char := range line {
			if char == '#' {
				monsterOffsets = append(monsterOffsets, [2]int{r, c})
			}
			monsterLength = c + 1
		}
		monsterHeight++
	}

	var monsterStartingCoords [][2]int
	for r := 0; r < len(image)-monsterHeight+1; r++ {
		for c := 0; c < len(image[0])-monsterLength+1; c++ {
			monsterFound := true
			for _, diff := range monsterOffsets {
				rowToCheck := r + diff[0]
				colToCheck := c + diff[1]
				if image[rowToCheck][colToCheck] != "#" {
					monsterFound = false
				}
			}
			if monsterFound {
				monsterStartingCoords = append(monsterStartingCoords, [2]int{r, c})
			}
		}
	}

	var monsterCoords [][2]int
	for _, startingCoord := range monsterStartingCoords {
		for _, diff := range monsterOffsets {
			monsterCoords = append(monsterCoords, [2]int{startingCoord[0] + diff[0], startingCoord[1] + diff[1]})
		}
	}

	return monsterCoords
}

func allGridOrientations(grid [][]string) [][][]string {
	orientations := [][][]string{grid}

	for i := 0; i < 3; i++ {
		orientations = append(orientations, rotateStringGrid(orientations[len(orientations)-1]))
	}

	for i := 0; i < 4; i++ {
		orientations = append(orientations, mirrorStringGrid(orientations[i]))
	}

	return orientations
}

func rotateStringGrid(grid [][]string) [][]string {
	rotated := make([][]string, len(grid[0]))
	for i := range rotated {
		rotated[i] = make([]string, len(grid))
	}

	for i := 0; i < len(grid); i++ {
		for j := 0; j < len(grid[0]); j++ {
			rotated[len(grid[0])-1-j][i] = grid[i][j]
		}
	}
	return rotated
}

func mirrorStringGrid(grid [][]string) (flipped [][]string) {
	for i := range grid {
		flipped = append(flipped, []string{})
		for j := len(grid[i]) - 1; j >= 0; j-- {
			flipped[i] = append(flipped[i], grid[i][j])
		}
	}
	return flipped
}