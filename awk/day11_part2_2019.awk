BEGIN {
    RS = ","
    while ((getline val < "input.txt") > 0) orig[++n] = val + 0
    solve(0)
    solve(1)
}

function solve(start,   m, c, p, i, r, x, y, d, q, o, m1, m2, m3, v1, v2, a1, a3, mx, Mx, my, My, str, t, g, a, j) {
    split("", m); split("", c); split("", p)
    for (i = 1; i <= n; i++) m[i-1] = orig[i]
    i = r = x = y = d = q = 0
    c["0,0"] = start
    while (1) {
        o = m[i] % 100
        if (o == 99) break
        m1 = int(m[i] / 100) % 10; m2 = int(m[i] / 1000) % 10; m3 = int(m[i] / 10000) % 10
        v1 = (m1 == 1 ? m[i+1] : (m1 == 2 ? m[r+m[i+1]] : m[m[i+1]])) + 0
        v2 = (m2 == 1 ? m[i+2] : (m2 == 2 ? m[r+m[i+2]] : m[m[i+2]])) + 0
        a1 = (m1 == 2 ? r + m[i+1] : m[i+1]); a3 = (m3 == 2 ? r + m[i+3] : m[i+3])
        if (o == 1) { m[a3] = v1 + v2; i += 4 }
        else if (o == 2) { m[a3] = v1 * v2; i += 4 }
        else if (o == 3) { m[a1] = (c[x "," y] == 1 ? 1 : 0); i += 2 }
        else if (o == 4) {
            if (q == 0) { c[x "," y] = v1; p[x "," y] = 1; q = 1 }
            else { d = (v1 == 0 ? (d + 3) % 4 : (d + 1) % 4); if (d == 0) y--; else if (d == 1) x++; else if (d == 2) y++; else x--; q = 0 }
            i += 2
        }
        else if (o == 5) i = (v1 != 0 ? v2 : i + 3)
        else if (o == 6) i = (v1 == 0 ? v2 : i + 3)
        else if (o == 7) { m[a3] = (v1 < v2 ? 1 : 0); i += 4 }
        else if (o == 8) { m[a3] = (v1 == v2 ? 1 : 0); i += 4 }
        else if (o == 9) { r += v1; i += 2 }
        else break
    }
    if (start == 0) {
        t = 0; for (a in p) t++; print "Part One: " t
    } else {
        mx = Mx = my = My = 0
        for (a in c) {
            split(a, g, ","); if (g[1] < mx) mx = g[1]; if (g[1] > Mx) Mx = g[1]
            if (g[2] < my) my = g[2]; if (g[2] > My) My = g[2]
        }
        print "Part Two:"
        for (j = my; j <= My; j++) {
            str = ""; for (k = mx; k <= Mx; k++) str = str (c[k "," j] == 1 ? "#" : " "); print str
        }
    }
}