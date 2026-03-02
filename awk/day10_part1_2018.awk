
BEGIN {
    while ((getline < "input.txt") > 0) {
        gsub(/[^0-9-]+/, " ")
        $0 = $0
        n++; x[n]=$(NF-3); y[n]=$(NF-2); u[n]=$(NF-1); v[n]=$NF
    }
    m = 1e15
    for (t = 0; ; t++) {
        ix = iy = 1e15; ax = ay = -1e15
        for (i = 1; i <= n; i++) {
            px = x[i] + u[i] * t; py = y[i] + v[i] * t
            if (px < ix) ix = px; if (px > ax) ax = px
            if (py < iy) iy = py; if (py > ay) ay = py
        }
        s = (ax - ix) + (ay - iy)
        if (s < m) { m = s; bt = t } else break
    }
    ix = iy = 1e15; ax = ay = -1e15
    for (i = 1; i <= n; i++) {
        px = x[i] + u[i] * bt; py = y[i] + v[i] * bt
        if (px < ix) ix = px; if (px > ax) ax = px
        if (py < iy) iy = py; if (py > ay) ay = py
        g[px, py] = 1
    }
    for (j = iy; j <= ay; j++) {
        for (i = ix; i <= ax; i++) printf ((i, j) in g ? "#" : " ")
        print ""
    }
}
