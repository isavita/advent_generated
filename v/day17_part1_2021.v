import os

fn main() {
    input := os.read_file('input.txt') or { return }
    parts := input.split(' ')
    x_range := parts[2][2..].split('..').map(it.int())
    y_range := parts[3][2..].split('..').map(it.int())

    mut highest_y := 0
    for vx in 0 .. x_range[1] + 1 {
        for vy in y_range[0] .. -y_range[0] + 1 {
            mut x := 0
            mut y := 0
            mut curr_vx := vx
            mut curr_vy := vy
            mut max_y := 0

            for {
                x += curr_vx
                y += curr_vy
                if y > max_y {
                    max_y = y
                }
                if curr_vx > 0 {
                    curr_vx--
                } else if curr_vx < 0 {
                    curr_vx++
                }
                curr_vy--

                if x >= x_range[0] && x <= x_range[1] && y >= y_range[0] && y <= y_range[1] {
                    if max_y > highest_y {
                        highest_y = max_y
                    }
                    break
                }
                if x > x_range[1] || y < y_range[0] {
                    break
                }
            }
        }
    }
    println(highest_y)
}