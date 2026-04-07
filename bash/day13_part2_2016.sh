
#!/bin/bash

fav=$(cat input.txt)

awk -v fav="$fav" '
function isWall(x, y, n, bits) {
    if (x < 0 || y < 0) return 1
    n = x*x + 3*x + 2*x*y + y + y*y + fav
    bits = 0
    while (n > 0) {
        if (n % 2 == 1) bits++
        n = int(n / 2)
    }
    return bits % 2 != 0
}

BEGIN {
    q[tail++] = 1; q[tail++] = 1; q[tail++] = 0
    visited["1,1"] = 1
    count = 1

    while (head < tail) {
        x = q[head++]; y = q[head++]; d = q[head++]
        if (d >= 50) continue

        split("1 -1 0 0", dx); split("0 0 1 -1", dy)
        for (i = 1; i <= 4; i++) {
            nx = x + dx[i]; ny = y + dy[i]
            if (nx >= 0 && ny >= 0 && !visited[nx","ny] && !isWall(nx, ny)) {
                visited[nx","ny] = 1
                q[tail++] = nx; q[tail++] = ny; q[tail++] = d + 1
                count++
            }
        }
    }
    print count
}'
