import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    target := input.trim_space().int()
    
    mut grid := map[string]int{}
    directions := [[1, 0], [0, 1], [-1, 0], [0, -1]]
    mut x, mut y := 0, 0
    mut dir_index := 0

    grid['0,0'] = 1

    for {
        x += directions[dir_index][0]
        y += directions[dir_index][1]

        mut sum := 0
        for dx in [-1, 0, 1] {
            for dy in [-1, 0, 1] {
                sum += grid['${x + dx},${y + dy}'] or { 0 }
            }
        }

        if sum > target {
            println(sum)
            return
        }

        grid['${x},${y}'] = sum

        next_x := x + directions[(dir_index + 1) % 4][0]
        next_y := y + directions[(dir_index + 1) % 4][1]
        if grid['${next_x},${next_y}'] == 0 {
            dir_index = (dir_index + 1) % 4
        }
    }
}