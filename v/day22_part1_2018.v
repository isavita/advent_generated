import os

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    depth, target := parse_input(data)

    cave := make_cave_system(depth, target)
    risk_level := calculate_risk_level(cave, target)
    println(risk_level)
}

fn parse_input(data string) (int, []int) {
    lines := data.split('\n')
    depth := lines[0].split(' ')[1].int()
    coords := lines[1].split(' ')[1].split(',')
    x := coords[0].int()
    y := coords[1].int()
    return depth, [x, y]
}

fn make_cave_system(depth int, target []int) [][]int {
    mut cave := [][]int{len: target[1] + 1, init: []int{len: target[0] + 1}}
    for y in 0 .. cave.len {
        for x in 0 .. cave[y].len {
            geologic_index := if (x == 0 && y == 0) || (x == target[0] && y == target[1]) {
                0
            } else if y == 0 {
                x * 16807
            } else if x == 0 {
                y * 48271
            } else {
                cave[y][x - 1] * cave[y - 1][x]
            }
            cave[y][x] = (geologic_index + depth) % 20183
        }
    }
    return cave
}

fn calculate_risk_level(cave [][]int, target []int) int {
    mut risk_level := 0
    for y in 0 .. target[1] + 1 {
        for x in 0 .. target[0] + 1 {
            risk_level += cave[y][x] % 3
        }
    }
    return risk_level
}