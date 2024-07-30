import os

fn main() {
    input := os.read_file('input.txt') or { return }
    parts := input.split(', ')
    x_range := parts[0][15..].split('..')
    y_range := parts[1][2..].split('..')
    x_min := x_range[0].int()
    x_max := x_range[1].int()
    y_min := y_range[0].int()
    y_max := y_range[1].int()

    mut velocities := map[string]bool{}
    for x_vel in -1000..1001 {
        for y_vel in -1000..1001 {
            mut x_pos := 0
            mut y_pos := 0
            mut cur_x_vel := x_vel
            mut cur_y_vel := y_vel
            mut in_target_area := false

            for {
                x_pos += cur_x_vel
                y_pos += cur_y_vel

                if x_pos >= x_min && x_pos <= x_max && y_pos >= y_min && y_pos <= y_max {
                    in_target_area = true
                    break
                }

                if is_moving_away(x_pos, y_pos, cur_x_vel, cur_y_vel, x_min, x_max, y_min, y_max) {
                    break
                }

                if cur_x_vel > 0 {
                    cur_x_vel--
                } else if cur_x_vel < 0 {
                    cur_x_vel++
                }
                cur_y_vel--
            }

            if in_target_area {
                velocities['$x_vel,$y_vel'] = true
            }
        }
    }

    println(velocities.len)
}

fn is_moving_away(x_pos int, y_pos int, x_vel int, y_vel int, x_min int, x_max int, y_min int, y_max int) bool {
    return (x_pos < x_min && x_vel < 0) || (x_pos > x_max && x_vel > 0) || (y_pos < y_min && y_vel < 0)
}