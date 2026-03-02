
BEGIN {
    for (i = 0; i < 16; i++) {
        v = sprintf("%X", i)
        h[v] = h[tolower(v)] = substr("0000000100100011010001010110011110001001101010111100110111101111", i * 4 + 1, 4)
    }
    if ((getline line < "input.txt") > 0) {
        for (i = 1; i <= length(line); i++) b = b h[substr(line, i, 1)]
        p = 1
        print parse()
    }
}

function get(n, i, r, s) {
    s = substr(b, p, n)
    p += n
    r = 0
    for (i = 1; i <= n; i++) r = r * 2 + substr(s, i, 1)
    return r
}

function parse(v, t, l, c, e, s, m) {
    v = get(3)
    t = get(3)
    s = v
    if (t == 4) {
        do {
            m = substr(b, p, 1)
            p += 5
        } while (m == "1")
    } else {
        if (get(1) == 0) {
            l = get(15)
            e = p + l
            while (p < e) s += parse()
        } else {
            c = get(11)
            while (c--) s += parse()
        }
    }
    return s
}
