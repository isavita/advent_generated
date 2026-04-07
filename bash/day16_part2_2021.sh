
#!/bin/bash
awk '
function b2d(s, i, v) {
    v = 0
    for (i = 1; i <= length(s); i++) v = v * 2 + substr(s, i, 1)
    return v
}
function p(idx, r,   t, l, n, i, c, v, s, u) {
    t = b2d(substr(B, idx + 3, 3))
    idx += 6
    if (t == 4) {
        s = 0
        while (substr(B, idx, 1) == "1") {
            s = s * 16 + b2d(substr(B, idx + 1, 4))
            idx += 5
        }
        r[1] = idx + 5
        r[2] = s * 16 + b2d(substr(B, idx + 1, 4))
        return
    }
    l = substr(B, idx, 1); idx++
    c = 0
    if (l == "0") {
        n = idx + 15 + b2d(substr(B, idx, 15)); idx += 15
        while (idx < n) { p(idx, u); idx = u[1]; v[++c] = u[2] }
    } else {
        n = b2d(substr(B, idx, 11)); idx += 11
        for (i = 0; i < n; i++) { p(idx, u); idx = u[1]; v[++c] = u[2] }
    }
    if (t == 0) { s = 0; for (i = 1; i <= c; i++) s += v[i] }
    else if (t == 1) { s = 1; for (i = 1; i <= c; i++) s *= v[i] }
    else if (t == 2) { s = v[1]; for (i = 2; i <= c; i++) if (v[i] < s) s = v[i] }
    else if (t == 3) { s = v[1]; for (i = 2; i <= c; i++) if (v[i] > s) s = v[i] }
    else if (t == 5) s = (v[1] > v[2] ? 1 : 0)
    else if (t == 6) s = (v[1] < v[2] ? 1 : 0)
    else if (t == 7) s = (v[1] == v[2] ? 1 : 0)
    r[1] = idx; r[2] = s
}
BEGIN {
    for (i = 0; i < 16; i++) H[sprintf("%X", i)] = substr("0000000100100011010001010110011110001001101010111100110111101111", i * 4 + 1, 4)
}
{
    for (i = 1; i <= length($0); i++) B = B H[toupper(substr($0, i, 1))]
    p(1, res)
    printf "%.0f\n", res[2]
}' input.txt
