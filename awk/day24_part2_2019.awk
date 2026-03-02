BEGIN {
    while ((getline < "input.txt") > 0) {
        split($0, a, "")
        for (i = 1; i <= 5; i++) if (a[i] == "#") g[0, (row * 5) + i - 1] = 1
        row++
    }
    minL = maxL = 0
    for (t = 1; t <= 200; t++) {
        minL--; maxL++
        for (l = minL; l <= maxL; l++) {
            for (i = 0; i < 25; i++) {
                if (i == 12) continue
                r = int(i / 5); c = i % 5; n = 0
                if (r == 0) n += ((l - 1, 7) in g)
                else if (i == 17) { for (k = 0; k < 5; k++) n += ((l + 1, 20 + k) in g) }
                else n += ((l, i - 5) in g)
                if (r == 4) n += ((l - 1, 17) in g)
                else if (i == 7) { for (k = 0; k < 5; k++) n += ((l + 1, k) in g) }
                else n += ((l, i + 5) in g)
                if (c == 0) n += ((l - 1, 11) in g)
                else if (i == 13) { for (k = 0; k < 5; k++) n += ((l + 1, k * 5 + 4) in g) }
                else n += ((l, i - 1) in g)
                if (c == 4) n += ((l - 1, 13) in g)
                else if (i == 11) { for (k = 0; k < 5; k++) n += ((l + 1, k * 5) in g) }
                else n += ((l, i + 1) in g)
                if (((l, i) in g) ? (n == 1) : (n == 1 || n == 2)) ng[l, i] = 1
            }
        }
        delete g
        for (k in ng) g[k] = 1
        delete ng
    }
    for (k in g) count++
    print count
}