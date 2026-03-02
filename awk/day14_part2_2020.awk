
BEGIN {
    FS = " = "
    CONVFMT = "%.0f"
    ARGV[1] = "input.txt"
    ARGC = 2
    for (i = 0; i < 36; i++) p[i] = 2 ^ (35 - i)
}

/^mask/ {
    mask = $2
    next
}

/^mem/ {
    match($1, /[0-9]+/)
    a = substr($1, RSTART, RLENGTH)
    v = $2
    xc = 0
    for (i = 0; i < 36; i++) {
        m = substr(mask, i + 1, 1)
        b = int(a / p[i]) % 2
        if (m == "1" && b == 0) a += p[i]
        if (m == "X") {
            if (b == 1) a -= p[i]
            xs[++xc] = p[i]
        }
    }
    f(a, 1, xc, v)
}

function f(a, k, n, v) {
    if (k > n) {
        mem[a] = v
        return
    }
    f(a, k + 1, n, v)
    f(a + xs[k], k + 1, n, v)
}

END {
    for (i in mem) s += mem[i]
    printf "%.0f\n", s
}
