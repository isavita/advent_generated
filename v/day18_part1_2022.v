import os

fn main() {
    file_path := 'input.txt'
    mut cubes := map[string]bool{}

    // Read input file and populate the cubes map
    if os.exists(file_path) {
        lines := os.read_lines(file_path) or {
            eprintln('Error reading file: $err')
            return
        }
        for line in lines {
            coords := line.split(',')
            x := coords[0].int()
            y := coords[1].int()
            z := coords[2].int()
            cubes['$x,$y,$z'] = true
        }
    } else {
        eprintln('File not found: $file_path')
        return
    }

    // Directions for adjacent cubes
    directions := [
        [1, 0, 0], [-1, 0, 0],
        [0, 1, 0], [0, -1, 0],
        [0, 0, 1], [0, 0, -1],
    ]

    mut surface_area := 0

    // Calculate surface area
    for key in cubes.keys() {
        coords := key.split(',')
        x := coords[0].int()
        y := coords[1].int()
        z := coords[2].int()

        for dir in directions {
            adjacent := '${x + dir[0]},${y + dir[1]},${z + dir[2]}'
            if !cubes[adjacent] {
                surface_area++
            }
        }
    }

    println(surface_area)
}