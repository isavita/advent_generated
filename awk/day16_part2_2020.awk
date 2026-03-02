BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
    sec = nr = nv = 0
}
/^$/ {
    sec++
    next
}
sec == 0 {
    split($0, a, ": ")
    names[nr] = a[1]
    split(a[2], b, " or ")
    split(b[1], c1, "-")
    split(b[2], c2, "-")
    r1s[nr] = c1[1]; r1e[nr] = c1[2]
    r2s[nr] = c2[1]; r2e[nr] = c2[2]
    nr++
}
sec == 1 && /^[0-9]/ {
    nc = split($0, myt, ",")
}
sec == 2 && /^[0-9]/ {
    n = split($0, f, ",")
    valid = 1
    for (i = 1; i <= n; i++) {
        any = 0; v = f[i] + 0
        for (j = 0; j < nr; j++) {
            if ((v >= r1s[j]+0 && v <= r1e[j]+0) || (v >= r2s[j]+0 && v <= r2e[j]+0)) {
                any = 1; break
            }
        }
        if (!any) { valid = 0; break }
    }
    if (valid) {
        nv++
        for (i = 1; i <= n; i++) vt[nv, i] = f[i]
    }
}
END {
    for (i = 0; i < nr; i++) {
        for (j = 1; j <= nc; j++) {
            can = 1
            for (k = 1; k <= nv; k++) {
                v = vt[k, j] + 0
                if (!((v >= r1s[i]+0 && v <= r1e[i]+0) || (v >= r2s[i]+0 && v <= r2e[i]+0))) {
                    can = 0; break
                }
            }
            if (can) {
                pos[i, j] = 1
                pc[i]++
            }
        }
    }
    for (it = 0; it < nr; it++) {
        for (i = 0; i < nr; i++) {
            if (pc[i] == 1) {
                for (j = 1; j <= nc; j++) if (pos[i, j]) { col = j; break }
                res[i] = col
                for (r = 0; r < nr; r++) if (pos[r, col]) { pos[r, col] = 0; pc[r]-- }
            }
        }
    }
    ans = 1
    for (i = 0; i < nr; i++) {
        if (names[i] ~ /^departure/) ans *= myt[res[i]]
    }
    printf "%.0f\n", ans
}