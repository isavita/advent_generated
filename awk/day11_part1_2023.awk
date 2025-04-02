
function abs_val(x) {
    return x < 0 ? -x : x
}

function calculate_length(x1, y1, x2, y2) {
    dx = abs_val(x2 - x1)
    dy = abs_val(y2 - y1)
    return dx + dy
}

function solve(expansion_factor, file) {
    width = 0
    height = 0
    delete grid
    delete galaxies

    while (getline line < file) {
        height++
        if (width == 0) {
            width = length(line)
        }
        for (x = 1; x <= width; x++) {
            char = substr(line, x, 1)
            if (char != ".") {
                grid[x, height] = char
                galaxies[++num_galaxies, 1] = x
                galaxies[num_galaxies, 2] = height
            }
        }
    }
    close(file)

    delete empty_rows
    for (y = 1; y <= height; y++) {
        is_empty = 1
        for (x = 1; x <= width; x++) {
            if ((x, y) in grid) {
                is_empty = 0
                break
            }
        }
        if (is_empty) {
            empty_rows[y] = 1
        }
    }

    delete empty_cols
    for (x = 1; x <= width; x++) {
        is_empty = 1
        for (y = 1; y <= height; y++) {
            if ((x, y) in grid) {
                is_empty = 0
                break
            }
        }
        if (is_empty) {
            empty_cols[x] = 1
        }
    }

    sum = 0
    for (i = 1; i <= num_galaxies; i++) {
        x1 = galaxies[i, 1]
        y1 = galaxies[i, 2]
        new_x1 = x1
        new_y1 = y1
        for (x = 1; x < x1; x++) {
            if (x in empty_cols) {
                new_x1 += expansion_factor - 1
            }
        }
        for (y = 1; y < y1; y++) {
            if (y in empty_rows) {
                new_y1 += expansion_factor - 1
            }
        }

        for (j = i + 1; j <= num_galaxies; j++) {
            x2 = galaxies[j, 1]
            y2 = galaxies[j, 2]
            new_x2 = x2
            new_y2 = y2
            for (x = 1; x < x2; x++) {
                if (x in empty_cols) {
                    new_x2 += expansion_factor - 1
                }
            }
            for (y = 1; y < y2; y++) {
                if (y in empty_rows) {
                    new_y2 += expansion_factor - 1
                }
            }

            sum += calculate_length(new_x1, new_y1, new_x2, new_y2)
        }
    }

    return sum
}

BEGIN {
    print solve(2, "input.txt")
    exit
}
