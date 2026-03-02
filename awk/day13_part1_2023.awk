
function f(a, n,   i, j, k, l) {
    for (i = 1; i < n; i++) {
        k = 1
        l = (i < n - i) ? i : n - i
        for (j = 0; j < l; j++)
            if (a[i - j] != a[i + 1 + j]) {
                k = 0
                break
            }
        if (k) return i
    }
    return 0
}

function s() {
    if (R) res += f(c, C) + 100 * f(r, R)
    delete r
    delete c
    R = 0
}

BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}

!NF {
    s()
    next
}

{
    r[++R] = $0
    C = length($0)
    for (i = 1; i <= C; i++)
        c[i] = c[i] substr($0, i, 1)
}

END {
    s()
    print res
}
