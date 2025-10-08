
BEGIN {
    size = 0
    while ((getline line < "input.txt") > 0) {
        for (i = 1; i <= length(line); i++) {
            grid[size, i-1] = substr(line, i, 1) + 0
        }
        size++
        size = size
    }
    close("input.txt")

    for (i = 0; i < size; i++)
        for (j = 0; j < size; j++)
            for (k = 0; k < 4; k++) {
                dx = (k % 2) * (1 - 2 * int(k / 2))
                dy = ((k + 1) % 2) * (1 - 2 * int((k + 1) / 2))
                nx = i + dx
                ny = j + dy
                while (nx >= 0 && nx < size && ny >= 0 && ny < size) {
                    if (grid[nx, ny] >= grid[i, j]) break
                    nx += dx
                    ny += dy
                }
                if (nx < 0 || nx >= size || ny < 0 || ny >= size) visible[i, j] = 1
            }

    count = 0
    for (i = 0; i < size; i++)
        for (j = 0; j < size; j++)
            if (visible[i, j]) count++
    print count
}
