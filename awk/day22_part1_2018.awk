BEGIN {
    getline < "input.txt"
    split($0, a, " ")
    depth = a[2]
    getline < "input.txt"
    split($0, b, " ")
    split(b[2], coords, ",")
    target_x = coords[1]
    target_y = coords[2]
    
    risk_level = 0
    for (y = 0; y <= target_y; y++) {
        for (x = 0; x <= target_x; x++) {
            if (x == 0 && y == 0 || x == target_x && y == target_y) {
                geologic_index = 0
            } else if (y == 0) {
                geologic_index = x * 16807
            } else if (x == 0) {
                geologic_index = y * 48271
            } else {
                geologic_index = cave[(y * (target_x + 1)) + (x - 1)] * cave[((y - 1) * (target_x + 1)) + x]
            }
            cave[y * (target_x + 1) + x] = (geologic_index + depth) % 20183
            risk_level += cave[y * (target_x + 1) + x] % 3
        }
    }
    print risk_level
}