
function g(t, v) {
    if (t ~ /^o/) return
    c[t] = c[t] ? c[t] "," v : v
    if (++n[t] == 2) q[++e] = t
}

BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}

$1 == "value" {
    v[vi] = $2
    t[vi++] = $5 " " $6
}

$1 == "bot" {
    b = $1 " " $2
    l[b] = $6 " " $7
    h[b] = $11 " " $12
}

END {
    for (i = 0; i < vi; i++) g(t[i], v[i])
    for (s = 1; s <= e; s++) {
        b = q[s]
        split(c[b], a, ",")
        v1 = a[1] + 0
        v2 = a[2] + 0
        lo = v1 < v2 ? v1 : v2
        hi = v1 > v2 ? v1 : v2
        if (lo == 17 && hi == 61) {
            print b
            exit
        }
        g(l[b], lo)
        g(h[b], hi)
    }
}
