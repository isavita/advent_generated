
#!/usr/bin/awk -f

function countSetBits(num,    count, n) {
    n = int(num)
    count = 0
    while (n > 0) {
        if (n % 2 == 1) {
            count++
        }
        n = int(n / 2)
    }
    return count
}

function is_wall(x, y, favorite_number,    num, bits) {
    num = x*x + 3*x + 2*x*y + y + y*y + favorite_number
    bits = countSetBits(num)
    return (bits % 2 != 0)
}

BEGIN {
    if ((getline favorite_number < "input.txt") <= 0) {
         print "Error reading input.txt" > "/dev/stderr"
         exit 1
    }
    close("input.txt")

    target_x = 31
    target_y = 39

    start_x = 1
    start_y = 1

    head = 1
    tail = 1

    delete visited

    qx[tail] = start_x
    qy[tail] = start_y
    qs[tail] = 0
    visited[start_x SUBSEP start_y] = 1
    tail++

    split("1 -1 0 0", dx, " ")
    split("0 0 1 -1", dy, " ")

    while (head < tail) {
        curr_x = qx[head]
        curr_y = qy[head]
        curr_steps = qs[head]
        head++

        if (curr_x == target_x && curr_y == target_y) {
            print curr_steps
            exit
        }

        for (i = 1; i <= 4; i++) {
            next_x = curr_x + dx[i]
            next_y = curr_y + dy[i]

            if (next_x < 0 || next_y < 0) {
                continue
            }

            key = next_x SUBSEP next_y

            if (key in visited || is_wall(next_x, next_y, favorite_number)) {
                continue
            }

            visited[key] = 1
            qx[tail] = next_x
            qy[tail] = next_y
            qs[tail] = curr_steps + 1
            tail++
        }
    }
}
