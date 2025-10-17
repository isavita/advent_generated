
import os

const grid_size = 300

fn main() {
    input := os.read_file('input.txt') or { panic('missing input.txt') }
    serial := input.int()

    mut grid := [][]int{len: grid_size, init: []int{len: grid_size}}
    for y in 0 .. grid_size {
        for x in 0 .. grid_size {
            rack_id := x + 11
            mut power := rack_id * (y + 1)
            power += serial
            power *= rack_id
            power = (power / 100) % 10 - 5
            grid[y][x] = power
        }
    }

    mut sum := [][]int{len: grid_size + 1, init: []int{len: grid_size + 1}}
    for y in 1 .. grid_size + 1 {
        for x in 1 .. grid_size + 1 {
            sum[y][x] = grid[y - 1][x - 1] + sum[y - 1][x] + sum[y][x - 1] - sum[y - 1][x - 1]
        }
    }

    mut max_power := -2147483648
    mut max_x, mut max_y, mut max_size := 0, 0, 0
    for size in 1 .. grid_size + 1 {
        for y in 1 .. grid_size - size + 2 {
            for x in 1 .. grid_size - size + 2 {
                total := sum[y + size - 1][x + size - 1] - sum[y + size - 1][x - 1] -
                         sum[y - 1][x + size - 1] + sum[y - 1][x - 1]
                if total > max_power {
                    max_power = total
                    max_x = x
                    max_y = y
                    max_size = size
                }
            }
        }
    }

    println('${max_x},${max_y},${max_size}')
}
