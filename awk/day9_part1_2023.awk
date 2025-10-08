
BEGIN {
    while ((getline < "input.txt") > 0) {
        n = split($0, a)
        lvl = 0
        for (i = 1; i <= n; i++) s[lvl,i] = a[i]
        sz[lvl] = n
        while (1) {
            allz = 1
            for (i = 1; i < sz[lvl]; i++) {
                s[lvl+1,i] = s[lvl,i+1] - s[lvl,i]
                if (s[lvl+1,i]) allz = 0
            }
            lvl++
            sz[lvl] = sz[lvl-1] - 1
            if (allz) break
        }
        pred = 0
        for (i = lvl; i >= 0; i--) pred += s[i, sz[i]]
        res += pred
    }
    print res
}
