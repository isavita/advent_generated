
#!/bin/bash
awk '
{
    gsub(/[(),]/, "")
    w[$1] = $2
    all[$1] = 1
    if ($3 == "->") {
        for (i = 4; i <= NF; i++) {
            h[$1] = h[$1] " " $i
            is_c[$i] = 1
        }
    }
}
END {
    for (n in all) if (!is_c[n]) root = n
    dfs(root)
}
function dfs(n,   a, i, cw, wc, tw, t, cor, c, cnt) {
    split(h[n], a)
    tw = w[n]
    for (i in a) {
        cw = dfs(a[i])
        tw += cw
        wc[cw]++
        v[a[i]] = cw
    }
    cnt = 0; for (cw in wc) cnt++
    if (cnt > 1 && !done) {
        for (cw in wc) if (wc[cw] == 1) t = cw; else cor = cw
        for (i in a) {
            c = a[i]
            if (v[c] == t) {
                print w[c] + (cor - t)
                done = 1; exit
            }
        }
    }
    return tw
}' input.txt
