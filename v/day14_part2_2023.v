
import os { read_file }

fn main() {
    data := read_file('input.txt') or { panic('Cannot read input.txt') }
    lines := data.trim_space().split_into_lines()
    mut grid := [][]u8{len: lines.len, init: []u8{len: lines[0].len}}
    
    // Parse grid
    for i, line in lines {
        for j, ch in line {
            grid[i][j] = u8(ch)
        }
    }
    
    // Part 1: Tilt north once
    mut grid1 := grid.clone()
    tilt_north(mut grid1)
    println('Part 1: ${calculate_load(grid1)}')
    
    // Part 2: Find cycle pattern
    mut seen := map[string]int{}
    mut loads := []int{}
    mut grid2 := grid.clone()
    
    for cycle := 0; cycle < 1000000000; cycle++ {
        // One full spin cycle
        tilt_north(mut grid2)
        tilt_west(mut grid2)
        tilt_south(mut grid2)
        tilt_east(mut grid2)
        
        key := grid_to_string(grid2)
        if key in seen {
            // Found a cycle
            start := seen[key]
            cycle_len := cycle - start
            remaining := 1000000000 - cycle - 1
            final_idx := start + (remaining % cycle_len)
            println('Part 2: ${loads[final_idx]}')
            break
        }
        
        seen[key] = cycle
        loads << calculate_load(grid2)
    }
}

fn grid_to_string(grid [][]u8) string {
    mut result := []u8{}
    for row in grid {
        result << row.clone()
        result << `\n`
    }
    return result.bytestr()
}

fn tilt_north(mut grid [][]u8) {
    rows := grid.len
    cols := grid[0].len
    
    for j in 0..cols {
        mut write_pos := 0
        for i in 0..rows {
            match grid[i][j] {
                `O` {
                    if i != write_pos {
                        grid[write_pos][j] = `O`
                        grid[i][j] = `.`
                    }
                    write_pos++
                }
                `#` {
                    write_pos = i + 1
                }
                else {}
            }
        }
    }
}

fn tilt_south(mut grid [][]u8) {
    rows := grid.len
    cols := grid[0].len
    
    for j in 0..cols {
        mut write_pos := rows - 1
        for i := rows - 1; i >= 0; i-- {
            match grid[i][j] {
                `O` {
                    if i != write_pos {
                        grid[write_pos][j] = `O`
                        grid[i][j] = `.`
                    }
                    write_pos--
                }
                `#` {
                    write_pos = i - 1
                }
                else {}
            }
        }
    }
}

fn tilt_west(mut grid [][]u8) {
    rows := grid.len
    cols := grid[0].len
    
    for i in 0..rows {
        mut write_pos := 0
        for j in 0..cols {
            match grid[i][j] {
                `O` {
                    if j != write_pos {
                        grid[i][write_pos] = `O`
                        grid[i][j] = `.`
                    }
                    write_pos++
                }
                `#` {
                    write_pos = j + 1
                }
                else {}
            }
        }
    }
}

fn tilt_east(mut grid [][]u8) {
    rows := grid.len
    cols := grid[0].len
    
    for i in 0..rows {
        mut write_pos := cols - 1
        for j := cols - 1; j >= 0; j-- {
            match grid[i][j] {
                `O` {
                    if j != write_pos {
                        grid[i][write_pos] = `O`
                        grid[i][j] = `.`
                    }
                    write_pos--
                }
                `#` {
                    write_pos = j - 1
                }
                else {}
            }
        }
    }
}

fn calculate_load(grid [][]u8) int {
    mut load := 0
    rows := grid.len
    for i, row in grid {
        for ch in row {
            if ch == `O` {
                load += rows - i
            }
        }
    }
    return load
}
