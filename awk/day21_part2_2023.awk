
BEGIN {
    maxSize = 0
    numIterations = 26501365
    while (getline < "input.txt" > 0) {
        maxSize++
        gardenInput[maxSize] = $0
        for (x = 1; x <= length($0); x++) {
            c = substr($0, x, 1)
            if (c != "#") {
                garden[x, maxSize] = 1
            }
            if (c == "S") {
                start_x = x
                start_y = maxSize
            }
        }
    }
    close("input.txt")

    queue[start_x, start_y] = 1
    for (i = 0; i < 3 * maxSize; i++) {
        if ((i % maxSize) == (maxSize - 1) / 2) {
            done[length(done) + 1] = length(queue)
        }
        if (length(done) == 3) {
            break
        }
        delete newQueue
        for (point in queue) {
            split(point, coords, SUBSEP)
            x = coords[1]
            y = coords[2]
            for (dir_x = -1; dir_x <= 1; dir_x++) {
                for (dir_y = -1; dir_y <= 1; dir_y++) {
                    if (abs(dir_x) == abs(dir_y)) continue
                    new_x = x + dir_x
                    new_y = y + dir_y
                    mod_x = (new_x + 10 * maxSize) % maxSize
                    if (mod_x == 0) mod_x = maxSize
                    mod_y = (new_y + 10 * maxSize) % maxSize
                    if (mod_y == 0) mod_y = maxSize
                    if (garden[mod_x, mod_y]) {
                        newQueue[new_x, new_y] = 1
                    }
                }
            }
        }
        delete queue
        for (point in newQueue) {
            queue[point] = 1
        }
    }

    n = int(numIterations / maxSize)
    a = done[1]
    b = done[2]
    c = done[3]
    sum = a + n * (b - a + ((n - 1) * (c - 2 * b + a) / 2))

    print sum
}

function abs(x) {
    return x < 0 ? -x : x
}
