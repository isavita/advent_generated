
BEGIN {
    while ((getline < "input.txt") > 0) {
        if (NF == 1) {
            t = $1
            l = substr(t, length(t), 1)
            for (i = 1; i < length(t); i++) c[substr(t, i, 2)]++
        } else if (NF == 3) {
            p1[$1] = substr($1, 1, 1) $3
            p2[$1] = $3 substr($1, 2, 1)
        }
    }
    for (i = 0; i < 40; i++) {
        for (p in c) {
            n[p1[p]] += c[p]
            n[p2[p]] += c[p]
        }
        delete c
        for (p in n) c[p] = n[p]
        delete n
    }
    for (p in c) f[substr(p, 1, 1)] += c[p]
    f[l]++
    for (i in f) {
        if (max == "" || f[i] > max) max = f[i]
        if (min == "" || f[i] < min) min = f[i]
    }
    printf "%.0f\n", max - min
}
