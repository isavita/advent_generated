function find(i, p_i, t) {
    p_i = i
    while (par[p_i] != p_i) p_i = par[p_i]
    while (par[i] != p_i) { t = par[i]; par[i] = p_i; i = t }
    return p_i
}
BEGIN {
    inf = "input.txt"
    while ((getline < inf) > 0) {
        if (split($0, a, ",") >= 3) {
            x[n]=0+a[1]; y[n]=0+a[2]; z[n]=0+a[3]; par[n]=n; sz[n]=1; n++
        }
    }
    if (n < 2) {
        print "Not enough points to form circuits."; exit
    }
    tf = "edges.tmp"
    for (i = 0; i < n; i++)
        for (j = i + 1; j < n; j++)
            printf "%.0f %d %d\n", (x[i]-x[j])^2+(y[i]-y[j])^2+(z[i]-z[j])^2, i, j > tf
    close(tf)
    cmd = "LC_ALL=C sort -n " tf " | head -n 1000"
    while ((cmd | getline l) > 0) {
        split(l, a); u = find(a[2]); v = find(a[3])
        if (u != v) {
            if (sz[u] < sz[v]) { t = u; u = v; v = t }
            par[v] = u; sz[u] += sz[v]
        }
    }
    close(cmd)
    for (i = 0; i < n; i++) {
        if (par[i] == i) {
            v = sz[i]
            if (v > t1) { t3 = t2; t2 = t1; t1 = v }
            else if (v > t2) { t3 = t2; t2 = v }
            else if (v > t3) { t3 = v }
        }
    }
    res = 1
    if (t1 > 0) res *= t1
    if (t2 > 0) res *= t2
    if (t3 > 0) res *= t3
    printf "Product of three largest circuit sizes: %.0f\n", res
    system("rm " tf)
}