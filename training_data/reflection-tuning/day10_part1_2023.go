package main

import (
	"bufio"
	"fmt"
	"os"
)

type Position struct {
	x, y, dist int
}

func main() {
	grid := readInput("input.txt")
	startX, startY := findStart(grid)
	startType := determineStartType(grid, startX, startY)
	grid[startY][startX] = startType
	
	maxDist := bfs(grid, startX, startY)
	fmt.Println(maxDist)
}

func readInput(filename string) [][]byte {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	var grid [][]byte
	for scanner.Scan() {
		grid = append(grid, []byte(scanner.Text()))
	}
	return grid
}

func findStart(grid [][]byte) (int, int) {
	for y, row := range grid {
		for x, cell := range row {
			if cell == 'S' {
				return x, y
			}
		}
	}
	return -1, -1
}

func determineStartType(grid [][]byte, x, y int) byte {
	north := y > 0 && (grid[y-1][x] == '|' || grid[y-1][x] == '7' || grid[y-1][x] == 'F')
	south := y < len(grid)-1 && (grid[y+1][x] == '|' || grid[y+1][x] == 'L' || grid[y+1][x] == 'J')
	west := x > 0 && (grid[y][x-1] == '-' || grid[y][x-1] == 'L' || grid[y][x-1] == 'F')
	east := x < len(grid[0])-1 && (grid[y][x+1] == '-' || grid[y][x+1] == '7' || grid[y][x+1] == 'J')

	if north && south {
		return '|'
	} else if east && west {
		return '-'
	} else if north && east {
		return 'L'
	} else if north && west {
		return 'J'
	} else if south && west {
		return '7'
	} else {
		return 'F'
	}
}

func bfs(grid [][]byte, startX, startY int) int {
	queue := []Position{{startX, startY, 0}}
	visited := make(map[Position]bool)
	maxDist := 0

	for len(queue) > 0 {
		curr := queue[0]
		queue = queue[1:]

		if curr.dist > maxDist {
			maxDist = curr.dist
		}

		for _, next := range getNextPositions(grid, curr.x, curr.y) {
			if !visited[next] {
				visited[next] = true
				queue = append(queue, Position{next.x, next.y, curr.dist + 1})
			}
		}
	}

	return maxDist
}

func getNextPositions(grid [][]byte, x, y int) []Position {
	var positions []Position
	switch grid[y][x] {
	case '|':
		positions = append(positions, Position{x, y - 1, 0}, Position{x, y + 1, 0})
	case '-':
		positions = append(positions, Position{x - 1, y, 0}, Position{x + 1, y, 0})
	case 'L':
		positions = append(positions, Position{x, y - 1, 0}, Position{x + 1, y, 0})
	case 'J':
		positions = append(positions, Position{x, y - 1, 0}, Position{x - 1, y, 0})
	case '7':
		positions = append(positions, Position{x, y + 1, 0}, Position{x - 1, y, 0})
	case 'F':
		positions = append(positions, Position{x, y + 1, 0}, Position{x + 1, y, 0})
	}
	return positions
}
