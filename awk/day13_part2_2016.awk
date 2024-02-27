
#!/usr/bin/awk -f

BEGIN {
    favoriteNumber = 1362
    start_x = 1
    start_y = 1
    maxSteps = 50
}

function isWall(x, y) {
    num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
    bits = 0
    while (num > 0) {
        if (num%2 == 1) {
            bits++
        }
        num = int(num/2)
    }
    return bits%2 != 0
}

function bfsMaxSteps(start_x, start_y, maxSteps) {
    visited[start_x, start_y] = 1
    queue[tail++] = start_x
    queue[tail++] = start_y
    steps = 0

    while ((head < tail) && (steps < maxSteps)) {
        size = tail - head
        for (i = 0; i < size; i += 2) {
            x = queue[head++]
            y = queue[head++]

            for (j = 1; j >= -1; j -= 2) {
                nx = x + j
                ny = y
                if ((nx >= 0) && (ny >= 0) && (!isWall(nx, ny)) && (!visited[nx, ny])) {
                    visited[nx, ny] = 1
                    queue[tail++] = nx
                    queue[tail++] = ny
                }

                nx = x
                ny = y + j
                if ((nx >= 0) && (ny >= 0) && (!isWall(nx, ny)) && (!visited[nx, ny])) {
                    visited[nx, ny] = 1
                    queue[tail++] = nx
                    queue[tail++] = ny
                }
            }
        }
        steps++
    }

    return length(visited)
}

{
    print bfsMaxSteps(start_x, start_y, maxSteps)
}
