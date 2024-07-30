import os

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    heightmap := data.split('\n').filter(it.len > 0).map(fn (line string) []int {
        return line.split('').map(it.int())
    })

    mut total_risk_level := 0
    for y, row in heightmap {
        for x, height in row {
            if is_low_point(heightmap, x, y) {
                total_risk_level += 1 + height
            }
        }
    }
    println(total_risk_level)
}

fn is_low_point(heightmap [][]int, x int, y int) bool {
    height := heightmap[y][x]
    return !(x > 0 && heightmap[y][x - 1] <= height) &&
           !(x < heightmap[y].len - 1 && heightmap[y][x + 1] <= height) &&
           !(y > 0 && heightmap[y - 1][x] <= height) &&
           !(y < heightmap.len - 1 && heightmap[y + 1][x] <= height)
}