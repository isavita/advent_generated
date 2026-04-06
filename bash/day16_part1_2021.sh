
awk '
function g(n, i, r, s) {
    s = substr(b, p, n); p += n
    r = 0; for (i = 1; i <= n; i++) r = r * 2 + substr(s, i, 1)
    return r
}
function f(v, t, i, l, e, c, s) {
    v = g(3); t = g(3); s = v
    if (t == 4) {
        do { i = substr(b, p, 1); p += 5 } while (i == "1")
    } else if (g(1) == 0) {
        l = g(15); e = p + l
        while (p < e) s += f()
    } else {
        c = g(11)
        while (c--) s += f()
    }
    return s
}
BEGIN {
    for (i = 0; i < 16; i++) {
        x = sprintf("%X", i)
        h[x] = h[tolower(x)] = substr("0000000100100011010001010110011110001001101010111100110111101111", i * 4 + 1, 4)
    }
    if ((getline l < "input.txt") > 0) {
        for (i = 1; i <= length(l); i++) b = b h[substr(l, i, 1)]
        p = 1
        print f()
    }
}'
