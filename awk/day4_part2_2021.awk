
BEGIN {
    ARGC = 2
    ARGV[1] = "input.txt"
}
NR == 1 {
    n = split($0, drw, ",")
    next
}
NF == 5 {
    bi = int(bc / 5)
    ri = bc % 5
    bc++
    for (i = 1; i <= 5; i++) {
        v = $i
        sum[bi] += v
        c = ++lt[v]
        lb[v, c] = bi
        lr[v, c] = ri
        lc[v, c] = i - 1
    }
}
END {
    rem = int((bc - 1) / 5) + 1
    for (i = 1; i <= n; i++) {
        d = drw[i]
        for (m = 1; m <= lt[d]; m++) {
            bi = lb[d, m]
            ri = lr[d, m]
            ci = lc[d, m]
            if (!won[bi] && !pk[bi, ri, ci]) {
                pk[bi, ri, ci] = 1
                sum[bi] -= d
                if (++rh[bi, ri] == 5 || ++ch[bi, ci] == 5) {
                    won[bi] = 1
                    if (--rem == 0) {
                        print sum[bi] * d
                        exit
                    }
                }
            }
        }
    }
}
