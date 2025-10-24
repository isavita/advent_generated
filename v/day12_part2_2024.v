
import os

struct Region {
mut:
    plant   u8
    area    int
    perimeter int
    sides   int
}

fn main() {
    content := os.read_file('input.txt') or { panic('Failed to read input.txt') }
    lines := content.trim('\n').split('\n')
    rows := lines.len
    cols := if rows > 0 { lines[0].len } else { 0 }
    
    mut grid := [][]u8{}
    for line in lines {
        grid << line.bytes()
    }
    
    mut visited := [][]bool{len: rows, init: []bool{len: cols}}
    mut regions := []Region{}
    
    // Find all regions
    for i in 0..rows {
        for j in 0..cols {
            if !visited[i][j] {
                region := flood_fill(grid, mut visited, i, j, rows, cols)
                regions << region
            }
        }
    }
    
    // Part 1: Sum of area * perimeter
    mut part1 := 0
    for region in regions {
        part1 += region.area * region.perimeter
    }
    
    // Part 2: Sum of area * sides
    mut part2 := 0
    for region in regions {
        part2 += region.area * region.sides
    }
    
    println('Part 1: $part1')
    println('Part 2: $part2')
}

fn flood_fill(grid [][]u8, mut visited [][]bool, start_i int, start_j int, rows int, cols int) Region {
    plant := grid[start_i][start_j]
    mut area := 0
    mut perimeter := 0
    mut queue := [][]int{len: 1, init: [start_i, start_j]}
    visited[start_i][start_j] = true
    
    for queue.len > 0 {
        pos := queue.pop()
        i, j := pos[0], pos[1]
        area++
        
        // Check 4 neighbors
        neighbors := [[i-1, j], [i+1, j], [i, j-1], [i, j+1]]
        for neighbor in neighbors {
            ni, nj := neighbor[0], neighbor[1]
            if ni < 0 || ni >= rows || nj < 0 || nj >= cols {
                perimeter++ // Out of bounds = fence needed
            } else if grid[ni][nj] != plant {
                perimeter++ // Different plant = fence needed
            } else if !visited[ni][nj] {
                visited[ni][nj] = true
                queue << [ni, nj]
            }
        }
    }
    
    sides := count_sides(grid, plant, start_i, start_j, rows, cols)
    
    return Region{plant, area, perimeter, sides}
}

fn count_sides(grid [][]u8, plant u8, start_i int, start_j int, rows int, cols int) int {
    // Create a set of all cells in this region
    mut region_cells := map[string]bool{}
    mut visited := [][]bool{len: rows, init: []bool{len: cols}}
    mut queue := [][]int{len: 1, init: [start_i, start_j]}
    visited[start_i][start_j] = true
    
    for queue.len > 0 {
        pos := queue.pop()
        i, j := pos[0], pos[1]
        region_cells['$i,$j'] = true
        
        neighbors := [[i-1, j], [i+1, j], [i, j-1], [i, j+1]]
        for neighbor in neighbors {
            ni, nj := neighbor[0], neighbor[1]
            if ni >= 0 && ni < rows && nj >= 0 && nj < cols && 
               grid[ni][nj] == plant && !visited[ni][nj] {
                visited[ni][nj] = true
                queue << [ni, nj]
            }
        }
    }
    
    mut sides := 0
    
    // Check each cell for corners (which indicate sides)
    for cell in region_cells.keys() {
        parts := cell.split(',')
        i := parts[0].int()
        j := parts[1].int()
        
        // Check each of the 4 corners
        // A corner exists when we have a 90-degree turn in the boundary
        
        // Check 4 corners: top-left, top-right, bottom-left, bottom-right
        corners := [
            ['top', 'left'],
            ['top', 'right'], 
            ['bottom', 'left'],
            ['bottom', 'right']
        ]
        
        for corner in corners {
            vertical := corner[0]
            horizontal := corner[1]
            
            mut has_corner := false
            
            // Check if this corner contributes to a side
            if vertical == 'top' && horizontal == 'left' {
                // Top-left corner
                top_in := i > 0 && region_cells['${i-1},$j'] or { false }
                left_in := j > 0 && region_cells['$i,${j-1}'] or { false }
                diag_in := i > 0 && j > 0 && region_cells['${i-1},${j-1}'] or { false }
                
                has_corner = (!top_in && !left_in) || (top_in && left_in && !diag_in)
            } else if vertical == 'top' && horizontal == 'right' {
                // Top-right corner
                top_in := i > 0 && region_cells['${i-1},$j'] or { false }
                right_in := j < grid[0].len-1 && region_cells['$i,${j+1}'] or { false }
                diag_in := i > 0 && j < grid[0].len-1 && region_cells['${i-1},${j+1}'] or { false }
                
                has_corner = (!top_in && !right_in) || (top_in && right_in && !diag_in)
            } else if vertical == 'bottom' && horizontal == 'left' {
                // Bottom-left corner
                bottom_in := i < grid.len-1 && region_cells['${i+1},$j'] or { false }
                left_in := j > 0 && region_cells['$i,${j-1}'] or { false }
                diag_in := i < grid.len-1 && j > 0 && region_cells['${i+1},${j-1}'] or { false }
                
                has_corner = (!bottom_in && !left_in) || (bottom_in && left_in && !diag_in)
            } else if vertical == 'bottom' && horizontal == 'right' {
                // Bottom-right corner
                bottom_in := i < grid.len-1 && region_cells['${i+1},$j'] or { false }
                right_in := j < grid[0].len-1 && region_cells['$i,${j+1}'] or { false }
                diag_in := i < grid.len-1 && j < grid[0].len-1 && region_cells['${i+1},${j+1}'] or { false }
                
                has_corner = (!bottom_in && !right_in) || (bottom_in && right_in && !diag_in)
            }
            
            if has_corner {
                sides++
            }
        }
    }
    
    return sides
}
