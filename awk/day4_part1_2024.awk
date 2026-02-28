
BEGIN{
    while ((getline line < "input.txt") > 0) grid[++rows] = line
    close("input.txt")
    word = "XMAS"
    n = length(word)
    split("0 1 0 -1 1 0 -1 0 1 1 1 -1 -1 -1 -1 1", d)
}
END{
    for (r = 1; r <= rows; r++) {
        for (c = 1; c <= length(grid[r]); c++) {
            if (substr(grid[r], c, 1) == substr(word, 1, 1)) {
                for (k = 1; k <= 16; k += 2) {
                    dr = d[k]; dc = d[k + 1]; ok = 1
                    for (i = 0; i < n; i++) {
                        rr = r + i * dr; cc = c + i * dc
                        if (rr < 1 || rr > rows || cc < 1 || cc > length(grid[rr]) || substr(grid[rr], cc, 1) != substr(word, i + 1, 1)) { ok = 0; break }
                    }
                    if (ok) count++
                }
            }
        }
    }
    print count
}
