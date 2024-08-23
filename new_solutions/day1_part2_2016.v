import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    directions := input.split(', ')

    mut x := 0
    mut y := 0
    mut dir := 0 // 0 = North, 1 = East, 2 = South, 3 = West
    mut visited := map[string]bool{}

    visited['0,0'] = true

    for d in directions {
        turn := d[0]
        distance := d[1..].int()

        dir = if turn == `R` { (dir + 1) % 4 } else { (dir + 3) % 4 }

        for _ in 0 .. distance {
            match dir {
                0 { y++ }
                1 { x++ }
                2 { y-- }
                3 { x-- }
                else {}
            }
            coord := '$x,$y'
            if visited[coord] {
                println(manhattan_distance(x, y))
                return
            }
            visited[coord] = true
        }
    }
}

fn manhattan_distance(x int, y int) int {
    return abs(x) + abs(y)
}

fn abs(value int) int {
    return if value < 0 { -value } else { value }
}