import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    directions := input.split(', ').map(it.trim_space())

    mut x, mut y := 0, 0
    mut facing := 0 // 0: North, 1: East, 2: South, 3: West

    for dir in directions {
        turn := dir[0]
        distance := dir[1..].int()

        facing = match turn {
            `L` { (facing + 3) % 4 }
            `R` { (facing + 1) % 4 }
            else { facing }
        }

        match facing {
            0 { y += distance } // North
            1 { x += distance } // East
            2 { y -= distance } // South
            3 { x -= distance } // West
            else {}
        }
    }

    println(abs(x) + abs(y))
}

fn abs(value int) int {
    return if value < 0 { -value } else { value }
}