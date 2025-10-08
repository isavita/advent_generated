
BEGIN {
    while ((getline line < "input.txt") > 0) {
        split(line, a, " ")
        springs = a[1]
        n = split(a[2], g, ",")
        for (i = 1; i <= n; i++) grp[i] = g[i]
        total += go(1, 1, 0)
        delete cache
    }
    print total
}

function go(p, gi, run,    c, key, res, need) {
    key = p SUBSEP gi SUBSEP run
    if (key in cache) return cache[key]
    if (p > length(springs)) {
        res = (gi > n && run == 0) || (gi == n && run == grp[n])
        cache[key] = res
        return res
    }
    res = 0
    c = substr(springs, p, 1)
    if (c == "." || c == "?") {
        if (run == 0) res += go(p + 1, gi, 0)
        else if (gi <= n && run == grp[gi]) res += go(p + 1, gi + 1, 0)
    }
    if (c == "#" || c == "?") {
        if (gi <= n && run < grp[gi]) res += go(p + 1, gi, run + 1)
    }
    cache[key] = res
    return res
}
