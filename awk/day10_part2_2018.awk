
BEGIN {
    while ((getline < "input.txt") > 0) {
        split($0, v, /[^0-9-]+/)
        n++; x[n]=v[2]; y[n]=v[3]; dx[n]=v[4]; dy[n]=v[5]
    }
    m = 1e30
    for (t = 0; ; t++) {
        lx = ly = 1e30; hx = hy = -1e30
        for (i = 1; i <= n; i++) {
            cx = x[i] + dx[i] * t
            cy = y[i] + dy[i] * t
            if (cx < lx) lx = cx
            if (cx > hx) hx = cx
            if (cy < ly) ly = cy
            if (cy > hy) hy = cy
        }
        s = (hx - lx) + (hy - ly)
        if (s <= m) {
            m = s
            b = t
        } else {
            print b
            exit
        }
    }
}
