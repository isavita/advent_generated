import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    lines := input.split('\n').filter(it.len > 0)
    wire1 := parse_wire_path(lines[0])
    wire2 := parse_wire_path(lines[1])

    intersections := find_intersections(wire1, wire2)
    min_steps := find_min_steps(wire1, wire2, intersections)
    println(min_steps)
}

fn parse_wire_path(line string) map[string]int {
    mut path := map[string]int{}
    mut x, mut y, mut steps := 0, 0, 0
    directions := line.split(',')

    for dir in directions {
        dir_type := dir[0]
        length := dir[1..].int()
        for _ in 0 .. length {
            match dir_type {
                `R` { x++ }
                `L` { x-- }
                `U` { y++ }
                `D` { y-- }
                else {}
            }
            steps++
            pos := '$x,$y'
            if pos !in path {
                path[pos] = steps
            }
        }
    }
    return path
}

fn find_intersections(wire1 map[string]int, wire2 map[string]int) []string {
    mut intersections := []string{}
    for pos in wire1.keys() {
        if pos in wire2 {
            intersections << pos
        }
    }
    return intersections
}

fn find_min_steps(wire1 map[string]int, wire2 map[string]int, intersections []string) int {
    mut min_steps := int(1e9)
    for pos in intersections {
        steps := wire1[pos] + wire2[pos]
        if steps < min_steps {
            min_steps = steps
        }
    }
    return min_steps
}