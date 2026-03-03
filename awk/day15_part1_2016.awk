BEGIN {
    while ((getline < "input.txt") > 0) {
        p[++n] = $4
        gsub(/[^0-9]/, "", $12)
        s[n] = $12
    }
    t = 0
    m = 1
    for (i = 1; i <= n; i++) {
        while ((s[i] + t + i) % p[i] != 0)
            t += m
        m *= p[i]
    }
    print t
}