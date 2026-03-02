
BEGIN {
    key = 811589153
    n = 0
    while ((getline < "input.txt") > 0) {
        val[n] = $1 * key
        pos[n] = n
        map[n] = n
        n++
    }
    for (r = 0; r < 10; r++) {
        for (i = 0; i < n; i++) {
            p = pos[i]
            v = val[i]
            q = (p + v) % (n - 1)
            if (q < 0) q += (n - 1)
            if (p < q) {
                for (j = p; j < q; j++) {
                    map[j] = map[j+1]
                    pos[map[j]] = j
                }
            } else if (p > q) {
                for (j = p; j > q; j--) {
                    map[j] = map[j-1]
                    pos[map[j]] = j
                }
            }
            map[q] = i
            pos[i] = q
        }
    }
    for (i = 0; i < n; i++) if (val[i] == 0) z = pos[i]
    print val[map[(z + 1000) % n]] + val[map[(z + 2000) % n]] + val[map[(z + 3000) % n]]
}
