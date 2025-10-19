
import os

const size = 50

struct Grid {
mut:
    grid [size][size]u8
}

fn read_input(path string) !Grid {
    content := os.read_file(path)!
    mut grid := Grid{}
    mut row := 0
    for line in content.split_into_lines() {
        if row >= size { break }
        for col in 0 .. line.len {
            if col >= size { break }
            grid.grid[row][col] = line[col]
        }
        row++
    }
    return grid
}

fn calc_hash(g Grid) int {
    mut h := 0
    for i in 0 .. size {
        for j in 0 .. size {
            h = h * 31 + int(g.grid[i][j])
        }
    }
    return h
}

fn transform(a Grid) Grid {
    mut b := Grid{}
    for i in 0 .. size {
        for j in 0 .. size {
            mut trees := 0
            mut yards := 0
            for dx in -1 .. 2 {
                for dy in -1 .. 2 {
                    if dx == 0 && dy == 0 { continue }
                    ni := i + dx
                    nj := j + dy
                    if ni >= 0 && ni < size && nj >= 0 && nj < size {
                        match a.grid[ni][nj] {
                            `|` { trees++ }
                            `#` { yards++ }
                            else {}
                        }
                    }
                }
            }
            match a.grid[i][j] {
                `.` { b.grid[i][j] = if trees >= 3 { `|` } else { `.` } }
                `|` { b.grid[i][j] = if yards >= 3 { `#` } else { `|` } }
                `#` { b.grid[i][j] = if yards > 0 && trees > 0 { `#` } else { `.` } }
                else { b.grid[i][j] = a.grid[i][j] }
            }
        }
    }
    return b
}

fn resources(g Grid) (int, int) {
    mut trees := 0
    mut yards := 0
    for i in 0 .. size {
        for j in 0 .. size {
            match g.grid[i][j] {
                `|` { trees++ }
                `#` { yards++ }
                else {}
            }
        }
    }
    return trees, yards
}

fn main() {
    mut g := read_input('input.txt') or { panic(err) }
    mut seen := map[int]int{}
    mut minute := 0
    mut cycle_start := 0
    mut cycle_len := 0
    for {
        h := calc_hash(g)
        if h in seen {
            cycle_start = seen[h]
            cycle_len = minute - cycle_start
            break
        }
        seen[h] = minute
        g = transform(g)
        minute++
    }
    remaining := (1_000_000_000 - cycle_start) % cycle_len
    for _ in 0 .. remaining {
        g = transform(g)
    }
    trees, yards := resources(g)
    println(trees * yards)
}
