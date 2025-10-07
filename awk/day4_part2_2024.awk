
BEGIN {
    while ((getline line < "input.txt") > 0) {
        row[++R] = line
        C = length(line)
    }
    for (x = 2; x < R; x++) {
        for (y = 2; y < C; y++) {
            if (substr(row[x], y, 1) == "A") {
                d1 = substr(row[x-1], y-1, 1) substr(row[x], y, 1) substr(row[x+1], y+1, 1)
                d2 = substr(row[x-1], y+1, 1) substr(row[x], y, 1) substr(row[x+1], y-1, 1)
                if ((d1 == "MAS" || d1 == "SAM") && (d2 == "MAS" || d2 == "SAM")) count++
            }
        }
    }
    print count
}
