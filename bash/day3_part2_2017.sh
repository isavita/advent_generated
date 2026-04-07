
awk '
BEGIN {
    getline target < "input.txt"
    grid[0, 0] = 1
    x = y = dx = 0
    dy = -1
    while (1) {
        if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)) {
            t = dx; dx = -dy; dy = t
        }
        x += dx; y += dy
        v = 0
        for (i = -1; i <= 1; i++)
            for (j = -1; j <= 1; j++)
                v += grid[x + i, y + j]
        grid[x, y] = v
        if (v > target) {
            print v
            exit
        }
    }
}'
