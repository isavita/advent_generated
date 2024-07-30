import os

fn main() {
    depths := read_input('input.txt')
    part_one_result := count_increases(depths)
    part_two_result := count_window_increases(depths)

    println('Part One: $part_one_result')
    println('Part Two: $part_two_result')
}

fn read_input(filename string) []int {
    content := os.read_file(filename) or { panic(err) }
    return content.split('\n').filter(it != '').map(it.int())
}

fn count_increases(depths []int) int {
    mut increases := 0
    for i in 1 .. depths.len {
        if depths[i] > depths[i - 1] {
            increases++
        }
    }
    return increases
}

fn count_window_increases(depths []int) int {
    mut increases := 0
    for i in 3 .. depths.len {
        if (depths[i - 2] + depths[i - 1] + depths[i]) > (depths[i - 3] + depths[i - 2] + depths[i - 1]) {
            increases++
        }
    }
    return increases
}