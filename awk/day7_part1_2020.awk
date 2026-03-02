BEGIN {
    while ((getline < "input.txt") > 0) {
        if ($5 == "no") continue
        o = $1 " " $2
        for (i = 6; i <= NF; i += 4) {
            r = $i " " $(i + 1)
            l[r] = l[r] (l[r] ? ";" : "") o
        }
    }
    q[1] = "shiny gold"; h = 1; t = 1
    while (h <= t) {
        c = q[h++]
        n = split(l[c], a, ";")
        for (i = 1; i <= n; i++) {
            if (!v[a[i]]) {
                v[a[i]] = 1
                q[++t] = a[i]
                cnt++
            }
        }
    }
    print cnt + 0
}