BEGIN {
    FS = ""
    ARGC = 2
    ARGV[1] = "input.txt"
}
{
    C = NF
    for (i = 1; i <= C; i++) g[NR - 1, i - 1] = $i
}
END {
    R = NR; TR = R * 5; TC = C * 5; target = TR * TC - 1
    for (r = 0; r < TR; r++) {
        ir = int(r / R); tr = r % R
        for (c = 0; c < TC; c++) {
            v = g[tr, c % C] + ir + int(c / C)
            cost[r * TC + c] = (v > 9 ? v - 9 : v)
        }
    }
    dist[0] = 0; q[0, 0] = 0; qc[0] = 1; max_d = 0
    for (d = 0; d <= max_d; d++) {
        for (i = 0; i < qc[d]; i++) {
            p = q[d, i]; delete q[d, i]
            if (d > dist[p]) continue
            if (p == target) { print d; exit }
            r = int(p / TC); c = p % TC
            if (r + 1 < TR) { p2 = p + TC; nd = d + cost[p2]; if (!(p2 in dist) || nd < dist[p2]) { dist[p2] = nd; q[nd, qc[nd]++] = p2; if (nd > max_d) max_d = nd } }
            if (c + 1 < TC) { p2 = p + 1; nd = d + cost[p2]; if (!(p2 in dist) || nd < dist[p2]) { dist[p2] = nd; q[nd, qc[nd]++] = p2; if (nd > max_d) max_d = nd } }
            if (r - 1 >= 0) { p2 = p - TC; nd = d + cost[p2]; if (!(p2 in dist) || nd < dist[p2]) { dist[p2] = nd; q[nd, qc[nd]++] = p2; if (nd > max_d) max_d = nd } }
            if (c - 1 >= 0) { p2 = p - 1; nd = d + cost[p2]; if (!(p2 in dist) || nd < dist[p2]) { dist[p2] = nd; q[nd, qc[nd]++] = p2; if (nd > max_d) max_d = nd } }
        }
        delete qc[d]
    }
}