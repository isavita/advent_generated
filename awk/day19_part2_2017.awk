
BEGIN {
    rows = 0
    while ((getline line < "input.txt") > 0) {
        grid[rows] = line
        if (length(line) > cols) cols = length(line)
        rows++
    }
    close("input.txt")

    x = index(grid[0], "|")
    y = 0
    dx = 0; dy = 1
    steps = 0

    while (1) {
        if (y < 0 || y >= rows || x < 1 || x > length(grid[y])) break
        cell = substr(grid[y], x, 1)
        if (cell == " ") break

        steps++
        if (cell == "+") {
            if (dx == 0) {
                dx = (x > 1 && (substr(grid[y], x-1, 1) ~ /[-A-Z]/)) ? -1 : 1
                dy = 0
            } else {
                dx = 0
                dy = (y > 0 && (substr(grid[y-1], x, 1) ~ /[|A-Z]/)) ? -1 : 1
            }
        }
        x += dx
        y += dy
    }

    print steps
}
