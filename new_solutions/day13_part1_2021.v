import os

fn main() {
    mut dots := map[string]bool{}
    mut folds := []string{}
    mut reading_folds := false

    lines := os.read_lines('input.txt') or { return }
    for line in lines {
        if line == '' {
            reading_folds = true
            continue
        }
        if reading_folds {
            folds << line
        } else {
            parts := line.split(',')
            x := parts[0].int()
            y := parts[1].int()
            dots['$x,$y'] = true
        }
    }

    first_fold := folds[0]
    parts := first_fold.split(' ')
    axis_value := parts[2].split('=')
    axis := axis_value[0]
    value := axis_value[1].int()

    mut new_dots := map[string]bool{}
    for dot in dots.keys() {
        coords := dot.split(',')
        mut x := coords[0].int()
        mut y := coords[1].int()
        if axis == 'y' {
            if y > value {
                y = 2 * value - y
            }
        } else {
            if x > value {
                x = 2 * value - x
            }
        }
        new_dots['$x,$y'] = true
    }

    println(new_dots.len)
}