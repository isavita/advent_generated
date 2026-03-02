
BEGIN {
    FS = "[: ]+"
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    u = $1
    if (!seen[u]++) nodes[++n] = u
    for (i = 2; i <= NF; i++) {
        v = $i
        if (v == "") continue
        if (!seen[v]++) nodes[++n] = v
        adj[u, ++deg[u]] = v
        adj[v, ++deg[v]] = u
    }
}
END {
    s = nodes[1]
    for (i = 2; i <= n; i++) {
        t = nodes[i]
        delete f; f_val = 0
        while (f_val < 4) {
            delete p; h = 1; tl = 1; q[tl++] = s; p[s] = "r"; found = 0
            while (h < tl) {
                c = q[h++]
                if (c == t) { found = 1; break }
                for (k = 1; k <= deg[c]; k++) {
                    v = adj[c, k]
                    if (!(v in p) && (1 - f[c, v] > 0)) {
                        p[v] = c; q[tl++] = v
                    }
                }
            }
            if (!found) break
            f_val++; c = t
            while (c != s) {
                pr = p[c]; f[pr, c]++; f[c, pr]--; c = pr
            }
        }
        if (f_val == 3) {
            delete r; h = 1; tl = 1; q[tl++] = s; r[s] = 1; cnt = 0
            while (h < tl) {
                c = q[h++]; cnt++
                for (k = 1; k <= deg[c]; k++) {
                    v = adj[c, k]
                    if (!(v in r) && (1 - f[c, v] > 0)) {
                        r[v] = 1; q[tl++] = v
                    }
                }
            }
            print cnt * (n - cnt); exit
        }
    }
}
