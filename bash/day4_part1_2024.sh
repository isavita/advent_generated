
#!/usr/bin/env bash
# Count occurrences of "XMAS" in all 8 directions in input.txt

awk '
BEGIN {
    word = "XMAS"
    wlen = length(word)
    dirs[1] = "0 1"
    dirs[2] = "1 0"
    dirs[3] = "1 1"
    dirs[4] = "-1 1"
    dirs[5] = "0 -1"
    dirs[6] = "-1 0"
    dirs[7] = "-1 -1"
    dirs[8] = "1 -1"
}
{
    grid[NR] = $0
}
END {
    rows = NR
    cols = length(grid[1])
    count = 0
    for (i = 1; i <= rows; i++) {
        for (j = 1; j <= cols; j++) {
            for (d = 1; d <= 8; d++) {
                split(dirs[d], dir, " ")
                dx = dir[1]
                dy = dir[2]
                x = i
                y = j
                ok = 1
                for (k = 1; k <= wlen; k++) {
                    if (x < 1 || x > rows || y < 1 || y > cols ||
                        substr(grid[x], y, 1) != substr(word, k, 1)) {
                        ok = 0
                        break
                    }
                    x += dx
                    y += dy
                }
                if (ok) count++
            }
        }
    }
    printf("XMAS appears %d times in the word search\n", count)
}
' input.txt
