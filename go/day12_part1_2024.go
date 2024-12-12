package main

import (
	"bufio"
	"fmt"
	"os"
)

type Point struct {
	x, y int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var grid []string
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	totalPrice := solve(grid)
	fmt.Println(totalPrice)
}

func solve(grid []string) int {
	rows := len(grid)
	if rows == 0 {
		return 0
	}
	cols := len(grid[0])

	visited := make([][]bool, rows)
	for i := range visited {
		visited[i] = make([]bool, cols)
	}

	totalPrice := 0
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if !visited[r][c] {
				area, perimeter := calculateRegion(grid, r, c, &visited)
				totalPrice += area * perimeter
			}
		}
	}
	return totalPrice
}

func calculateRegion(grid []string, row int, col int, visited *[][]bool) (int, int) {
	rows := len(grid)
	cols := len(grid[0])
	char := grid[row][col]
	area := 0
	perimeter := 0

	queue := []Point{{row, col}}
	(*visited)[row][col] = true

	for len(queue) > 0 {
		p := queue[0]
		queue = queue[1:]

		area++

		//Check if it's a border
		isBorder := false
		if p.x == 0 || p.x == rows - 1 || p.y == 0 || p.y == cols -1 {
			isBorder = true
		}

		// Check top
		if p.x > 0 {
			if grid[p.x - 1][p.y] != char {
				perimeter++
			} else if !(*visited)[p.x-1][p.y] {
				queue = append(queue, Point{p.x - 1, p.y})
				(*visited)[p.x-1][p.y] = true
			}
		} else if isBorder {
			perimeter++
		}
		// Check bottom
		if p.x < rows - 1 {
			if grid[p.x + 1][p.y] != char{
				perimeter++
			} else if !(*visited)[p.x+1][p.y] {
				queue = append(queue, Point{p.x + 1, p.y})
				(*visited)[p.x+1][p.y] = true
			}
		} else if isBorder {
			perimeter++
		}
		// Check left
		if p.y > 0 {
			if grid[p.x][p.y-1] != char {
				perimeter++
			} else if !(*visited)[p.x][p.y-1] {
				queue = append(queue, Point{p.x, p.y - 1})
				(*visited)[p.x][p.y-1] = true
			}
		} else if isBorder {
			perimeter++
		}
		// Check right
		if p.y < cols -1 {
			if grid[p.x][p.y+1] != char {
				perimeter++
			} else if !(*visited)[p.x][p.y+1] {
				queue = append(queue, Point{p.x, p.y + 1})
				(*visited)[p.x][p.y+1] = true
			}
		} else if isBorder {
			perimeter++
		}

	}
	return area, perimeter
}
