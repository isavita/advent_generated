import os

fn main() {
    mut grid := [][]bool{len: 5, init: []bool{len: 5}}
    mut seen := map[int]bool{}

    // Read the input file
    lines := os.read_lines('input.txt') or { panic(err) }
    for y in 0 .. lines.len {
        for x in 0 .. lines[y].len {
            grid[y][x] = lines[y][x] == `#`
        }
    }

    for {
        biodiversity := calculate_biodiversity(grid)
        if seen[biodiversity] {
            println(biodiversity)
            break
        }
        seen[biodiversity] = true
        grid = next_minute(grid)
    }
}

fn calculate_biodiversity(grid [][]bool) int {
    mut rating := 0
    for y in 0 .. grid.len {
        for x in 0 .. grid[y].len {
            if grid[y][x] {
                rating += 1 << (y * 5 + x)
            }
        }
    }
    return rating
}

fn next_minute(grid [][]bool) [][]bool {
    mut new_grid := [][]bool{len: 5, init: []bool{len: 5}}
    for y in 0 .. grid.len {
        for x in 0 .. grid[y].len {
            adjacent_bugs := count_adjacent_bugs(grid, x, y)
            new_grid[y][x] = (grid[y][x] && adjacent_bugs == 1) || (!grid[y][x] && (adjacent_bugs == 1 || adjacent_bugs == 2))
        }
    }
    return new_grid
}

fn count_adjacent_bugs(grid [][]bool, x int, y int) int {
    mut count := 0
    for dy in [-1, 1] {
        if y + dy >= 0 && y + dy < grid.len && grid[y + dy][x] {
            count++
        }
    }
    for dx in [-1, 1] {
        if x + dx >= 0 && x + dx < grid[y].len && grid[y][x + dx] {
            count++
        }
    }
    return count
}