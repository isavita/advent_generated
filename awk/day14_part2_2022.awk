BEGIN {
    FS = " -> "
    while ((getline < "input.txt") > 0) {
        for (i = 1; i < NF; i++) {
            split($i, a, ","); split($(i+1), b, ",")
            x = a[1]; y = a[2]; x2 = b[1]; y2 = b[2]
            if (y > my) my = y; if (y2 > my) my = y2
            dx = (x < x2) - (x > x2); dy = (y < y2) - (y > y2)
            while (1) {
                m[x, y] = 1
                if (x == x2 && y == y2) break
                x += dx; y += dy
            }
        }
    }
    f = my + 2
    while (m[500, 0] != 2) {
        x = 500; y = 0
        while (y < f - 1) {
            if (!m[x, y + 1]) y++
            else if (!m[x - 1, y + 1]) { y++; x-- }
            else if (!m[x + 1, y + 1]) { y++; x++ }
            else break
        }
        m[x, y] = 2; c++
    }
    print c
}