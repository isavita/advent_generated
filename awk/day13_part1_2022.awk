
#!/usr/bin/awk -f

function tk(s, a, i, c, d, t, n) {
    n = d = 0; t = 1
    for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        if (c == "[") d++
        else if (c == "]") d--
        if (c == "," && d == 0) {
            a[++n] = substr(s, t, i - t)
            t = i + 1
        }
    }
    if (length(s) >= t) a[++n] = substr(s, t)
    return n
}

function cp(s, t, i, A, B, n, m, r) {
    if (s ~ /^[0-9]+$/ && t ~ /^[0-9]+$/) return (s + 0 < t + 0 ? -1 : s + 0 > t + 0 ? 1 : 0)
    if (s ~ /^[0-9]+$/) return cp("[" s "]", t)
    if (t ~ /^[0-9]+$/) return cp(s, "[" t "]")
    n = tk(substr(s, 2, length(s) - 2), A)
    m = tk(substr(t, 2, length(t) - 2), B)
    for (i = 1; i <= n && i <= m; i++)
        if (r = cp(A[i], B[i])) return r
    return (n < m ? -1 : n > m ? 1 : 0)
}

BEGIN {
    RS = ""
    while ((getline < "input.txt") > 0) {
        j++
        if (cp($1, $2) == -1) sum += j
    }
    print sum + 0
}
