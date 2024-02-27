
{
    split($0, parts)
    split(parts[length(parts)-2], start, ",")
    split(parts[length(parts)], end, ",")
    
    for (x = start[1]; x <= end[1]; x++) {
        for (y = start[2]; y <= end[2]; y++) {
            if (index($0, "turn on") == 1) {
                grid[x, y] = 1
            } else if (index($0, "turn off") == 1) {
                grid[x, y] = 0
            } else if (index($0, "toggle") == 1) {
                grid[x, y] = !grid[x, y]
            }
        }
    }
}

END {
    count = 0
    for (x = 0; x < 1000; x++) {
        for (y = 0; y < 1000; y++) {
            if (grid[x, y] == 1) {
                count++
            }
        }
    }
    print count
}
