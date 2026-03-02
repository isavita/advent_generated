
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    C = length($0)
    for (i = 1; i <= C; i++) {
        g[NR - 1, i - 1] = substr($0, i, 1)
        dist[NR - 1, i - 1] = 1e12
    }
    R = NR
}
END {
    dist[0, 0] = 0
    q[0, 0] = "0 0"
    qc[0] = 1
    m = 0
    split("1 0 -1 0 0 1 0 -1", o)
    for (d = 0; d <= m; d++) {
        for (i = 0; i < qc[d]; i++) {
            split(q[d, i], p)
            r = p[1]; c = p[2]
            if (d > dist[r, c]) continue
            if (r == R - 1 && c == C - 1) {
                print d
                exit
            }
            for (j = 1; j < 8; j += 2) {
                nr = r + o[j]; nc = c + o[j + 1]
                if ((nr, nc) in g) {
                    v = d + g[nr, nc]
                    if (v < dist[nr, nc]) {
                        dist[nr, nc] = v
                        q[v, qc[v]++] = nr " " nc
                        if (v > m) m = v
                    }
                }
            }
        }
    }
}
