
# Read input from file
BEGIN {
    while ((getline line < "input.txt") > 0) {
        target = int(line)
    }
}

# Initialize variables
{
    grid[0, 0] = 1
    x = 0
    y = 0
    dx = 0
    dy = -1
}

# Main loop
{
    while (1) {
        if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1-y)) {
            tmp = dx
            dx = -dy
            dy = tmp
        }

        x += dx
        y += dy

        value = 0
        for (i = -1; i <= 1; i++) {
            for (j = -1; j <= 1; j++) {
                value += grid[x + i, y + j]
            }
        }
        grid[x, y] = value

        if (value > target) {
            print value
            exit
        }
    }
}
