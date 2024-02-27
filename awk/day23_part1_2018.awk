
BEGIN {
    while ((getline line < "input.txt") > 0) {
        split(line, coords, /pos=<|,|>, r=/)
        x = coords[2]
        y = coords[3]
        z = coords[4]
        radius = coords[5]
        nanobots[length(nanobots)] = x " " y " " z " " radius
    }
    
    for (i in nanobots) {
        split(nanobots[i], data, " ")
        x = data[1]
        y = data[2]
        z = data[3]
        radius = data[4]
        
        if (radius > strongestRadius) {
            strongestRadius = radius
            strongestX = x
            strongestY = y
            strongestZ = z
        }
    }
    
    for (i in nanobots) {
        split(nanobots[i], data, " ")
        x = data[1]
        y = data[2]
        z = data[3]
        radius = data[4]
        
        distance = abs(strongestX - x) + abs(strongestY - y) + abs(strongestZ - z)
        
        if (distance <= strongestRadius) {
            count++
        }
    }
    
    print count
}

function abs(x) {
    return x < 0 ? -x : x
}
