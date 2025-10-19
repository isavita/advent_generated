
import os

fn main() {
    content := os.read_file('input.txt') or { panic('input.txt') }
    lines := content.split_into_lines()
    rows := lines.len
    cols := lines[0].len
    mut heightmap := [][]int{len: rows}
    for y in 0..rows {
        heightmap[y] = []int{len: cols}
        for x in 0..cols {
            heightmap[y][x] = int(lines[y][x] - `0`)
        }
    }
    mut visited := []bool{len: rows*cols}
    mut sizes := []int{len: 0}
    for y in 0..rows {
        for x in 0..cols {
            if is_low(heightmap, x, y, rows, cols) {
                sizes << basin(heightmap, x, y, rows, cols, mut visited)
            }
        }
    }
    sizes.sort()
    println(sizes[sizes.len-3]*sizes[sizes.len-2]*sizes[sizes.len-1])
}

fn is_low(heightmap [][]int, x int, y int, rows int, cols int) bool {
    h := heightmap[y][x]
    return (x==0 || heightmap[y][x-1]>h)
        && (x==cols-1 || heightmap[y][x+1]>h)
        && (y==0 || heightmap[y-1][x]>h)
        && (y==rows-1 || heightmap[y+1][x]>h)
}

fn basin(heightmap [][]int, x int, y int, rows int, cols int, mut visited []bool) int {
    if visited[y*cols+x] || heightmap[y][x]==9 { return 0 }
    visited[y*cols+x] = true
    mut size := 1
    if x>0 { size += basin(heightmap, x-1, y, rows, cols, mut visited) }
    if x<cols-1 { size += basin(heightmap, x+1, y, rows, cols, mut visited) }
    if y>0 { size += basin(heightmap, x, y-1, rows, cols, mut visited) }
    if y<rows-1 { size += basin(heightmap, x, y+1, rows, cols, mut visited) }
    return size
}
