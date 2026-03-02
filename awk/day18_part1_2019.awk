BEGIN {
    while ((getline l < "input.txt") > 0) {
        y++
        for (x = 1; x <= length(l); x++) {
            v = substr(l, x, 1)
            g[y, x] = v
            if (v == "@") { sy = y; sx = x; g[y, x] = "." }
            if (v ~ /[a-z]/ && !(v in k)) { k[v] = kc; p[kc++] = 2^(kc-1) }
        }
    }
    t = 2^kc - 1
    q[h++] = sy "," sx ",0,0"
    vis[sy, sx, 0] = 1
    while (i < h) {
        split(q[i++], c, ",")
        y = c[1]; x = c[2]; m = c[3]; d = c[4]
        if (m == t) { print d; exit }
        for (j = 1; j <= 4; j++) {
            ny = y + (j==1?-1:j==2?1:0)
            nx = x + (j==3?-1:j==4?1:0)
            ch = g[ny, nx]; nm = m
            if (ch == "" || ch == "#") continue
            if (ch ~ /[a-z]/) {
                ki = k[ch]
                if (int(m / 2^ki) % 2 == 0) nm += 2^ki
            } else if (ch ~ /[A-Z]/) {
                ki = k[tolower(ch)]
                if (int(m / 2^ki) % 2 == 0) continue
            }
            if (!vis[ny, nx, nm]++) q[h++] = ny "," nx "," nm "," (d+1)
        }
    }
    print -1
}