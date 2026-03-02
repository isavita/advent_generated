
BEGIN {
    FS = ""
    ARGV[1] = "input.txt"
    ARGC = 2
}

function k(s1, s2, i, x, n) {
    x = 0
    n = length(s1)
    for (i = 1; i <= n; i++)
        if (substr(s1, i, 1) != substr(s2, i, 1)) x++
    return x
}

function p(i, j, d) {
    if (!r) return
    for (i = 1; i < r; i++) {
        d = 0
        for (j = 0; i - j > 0 && i + j + 1 <= r; j++)
            d += k(R[i - j], R[i + j + 1])
        if (d == 1) t += 100 * i
    }
    for (i = 1; i < m; i++) {
        d = 0
        for (j = 0; i - j > 0 && i + j + 1 <= m; j++)
            d += k(C[i - j], C[i + j + 1])
        if (d == 1) t += i
    }
    delete R
    delete C
    r = 0
}

NF {
    R[++r] = $0
    m = NF
    for (f = 1; f <= m; f++) C[f] = C[f] $f
}

!NF {
    p()
}

END {
    p()
    print t
}
