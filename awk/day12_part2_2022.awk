
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
    for (i = 0; i < 26; i++) h[sprintf("%c", 97 + i)] = i
}
{
    split($0, chars, "")
    for (i = 1; i <= length($0); i++) {
        v = chars[i]
        if (v == "S") v = "a"
        if (v == "E") { v = "z"; er = NR; ec = i }
        grid[NR, i] = v
    }
}
END {
    head = 1
    tail = 1
    q[1] = er SUBSEP ec
    dist[er, ec] = 0
    split("1 0 -1 0 0 1 0 -1", m)
    while (head <= tail) {
        split(q[head++], p, SUBSEP)
        r = p[1]; c = p[2]
        if (grid[r, c] == "a") {
            print dist[r, c]
            exit
        }
        for (i = 1; i <= 8; i += 2) {
            nr = r + m[i]; nc = c + m[i+1]
            if ((nr, nc) in grid && !((nr, nc) in dist)) {
                if (h[grid[r, c]] - h[grid[nr, nc]] <= 1) {
                    dist[nr, nc] = dist[r, c] + 1
                    q[++tail] = nr SUBSEP nc
                }
            }
        }
    }
}
