
#!/bin/bash
awk '
BEGIN {
    FS = ""
    for (i = 0; i < 26; i++) h[sprintf("%c", 97 + i)] = i
}
{
    for (i = 1; i <= NF; i++) {
        v = $i
        if (v == "S") v = "a"
        if (v == "E") { v = "z"; er = NR; ec = i }
        grid[NR, i] = v
    }
}
END {
    head = 1; tail = 1
    q[1] = er SUBSEP ec
    dist[er, ec] = 0
    while (head <= tail) {
        split(q[head++], p, SUBSEP)
        r = p[1]; c = p[2]
        if (grid[r, c] == "a") {
            print dist[r, c]
            exit
        }
        for (i = 1; i <= 4; i++) {
            nr = r + (i==1?1:(i==2?-1:0))
            nc = c + (i==3?1:(i==4?-1:0))
            if ((nr, nc) in grid && !((nr, nc) in dist)) {
                if (h[grid[r, c]] - h[grid[nr, nc]] <= 1) {
                    dist[nr, nc] = dist[r, c] + 1
                    q[++tail] = nr SUBSEP nc
                }
            }
        }
    }
}' input.txt
