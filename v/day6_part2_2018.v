
import os

struct Coord {
    x int
    y int
}

fn manhattan(a Coord, b Coord) int {
    return if a.x > b.x { a.x - b.x } else { b.x - a.x } +
           if a.y > b.y { a.y - b.y } else { b.y - a.y }
}

fn main() {
    txt := os.read_file('input.txt') or { panic(err) }
    mut coords := []Coord{}
    mut min_x, mut min_y := 2147483647, 2147483647
    mut max_x, mut max_y := -2147483648, -2147483648
    for line in txt.split_into_lines() {
        parts := line.split(',')
        x := parts[0].int()
        y := parts[1].trim_space().int()
        coords << Coord{x, y}
        if x < min_x { min_x = x }
        if y < min_y { min_y = y }
        if x > max_x { max_x = x }
        if y > max_y { max_y = y }
    }
    mut safe := 0
    for i := min_x; i <= max_x; i++ {
        for j := min_y; j <= max_y; j++ {
            mut sum := 0
            for c in coords {
                sum += manhattan(Coord{i, j}, c)
                if sum >= 10000 { break }
            }
            if sum < 10000 { safe++ }
        }
    }
    println(safe)
}
