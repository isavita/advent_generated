
{
    instruction = $0
    split(instruction, parts, " ")
    split(parts[length(parts)-2], start, ",")
    split(parts[length(parts)], end, ",")

    for (x = start[1]; x <= end[1]; x++) {
        for (y = start[2]; y <= end[2]; y++) {
            if (index(instruction, "turn on") == 1) {
                grid[x,y]++
            } else if (index(instruction, "turn off") == 1) {
                if (grid[x,y] > 0) {
                    grid[x,y]--
                }
            } else if (index(instruction, "toggle") == 1) {
                grid[x,y] += 2
            }
        }
    }
}

END {
    brightness = 0
    for (i = 1; i <= 1000; i++) {
        for (j = 1; j <= 1000; j++) {
            brightness += grid[i,j]
        }
    }
    print brightness
}
