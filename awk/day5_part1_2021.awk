
BEGIN {
    while ((getline < "input.txt") > 0) {
        split($0, line, " -> ")
        split(line[1], startCoords, ",")
        split(line[2], endCoords, ",")
        
        x1 = startCoords[1]
        y1 = startCoords[2]
        x2 = endCoords[1]
        y2 = endCoords[2]
        
        if (x1 == x2) {
            if (y1 > y2) {
                temp = y1
                y1 = y2
                y2 = temp
            }
            for (y = y1; y <= y2; y++) {
                grid[x1 "," y]++
            }
        } else if (y1 == y2) {
            if (x1 > x2) {
                temp = x1
                x1 = x2
                x2 = temp
            }
            for (x = x1; x <= x2; x++) {
                grid[x "," y1]++
            }
        }
    }
    
    overlapCount = 0
    for (point in grid) {
        if (grid[point] > 1) {
            overlapCount++
        }
    }
    
    print overlapCount
}
