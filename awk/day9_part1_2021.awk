
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    w = length($0)
    for (i = 1; i <= w; i++) g[NR, i] = substr($0, i, 1)
}
END {
    for (r = 1; r <= NR; r++) {
        for (c = 1; c <= w; c++) {
            v = g[r, c]
            if ((r > 1 && g[r-1, c] <= v) || (r < NR && g[r+1, c] <= v) || \
                (c > 1 && g[r, c-1] <= v) || (c < w && g[r, c+1] <= v)) continue
            s += v + 1
        }
    }
    print s + 0
}
