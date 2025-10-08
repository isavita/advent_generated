
BEGIN {
    while (getline < "input.txt" > 0) {
        split($0, a, "-")
        add(a[1], a[2]); add(a[2], a[1])
    }
    vis["start"] = 1
    walk("start")
    print ans
}
function add(x, y,    n) {
    n = ++cnt[x]; conn[x,n] = y
}
function walk(c,    i, t) {
    if (c == "end") { ++ans; return }
    for (i = 1; i <= cnt[c]; ++i) {
        t = conn[c,i]
        if (vis[t] && tolower(t) == t) continue
        vis[t] = 1
        walk(t)
        vis[t] = 0
    }
}
