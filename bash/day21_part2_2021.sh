
#!/bin/bash
awk '
function s(p1, p2, s1, s2,    k, i, r, w, np, res, b, w1, w2) {
    if (s1 >= 21) return "1 0"
    if (s2 >= 21) return "0 1"
    k = p1 SUBSEP p2 SUBSEP s1 SUBSEP s2
    if (k in m) return m[k]
    w1 = 0; w2 = 0
    for (i = 1; i <= 7; i++) {
        r = rs[i]; w = cs[i]
        np = (p1 + r - 1) % 10 + 1
        res = s(p2, np, s2, s1 + np)
        split(res, b)
        w1 += b[2] * w
        w2 += b[1] * w
    }
    return m[k] = w1 " " w2
}
BEGIN {
    CONVFMT = "%.0f"
    split("3 4 5 6 7 8 9", rs)
    split("1 3 6 7 6 3 1", cs)
}
/Player 1/ { p1 = $NF }
/Player 2/ { p2 = $NF }
END {
    split(s(p1, p2, 0, 0), f)
    v1 = f[1] + 0; v2 = f[2] + 0
    printf "%.0f\n", (v1 > v2 ? v1 : v2)
}
' input.txt
