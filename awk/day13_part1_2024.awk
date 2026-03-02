BEGIN {
    RS = ""
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    split($0, v, /[^0-9]+/)
    best = -1
    for (a = 0; a <= 100; a++) {
        for (b = 0; b <= 100; b++) {
            if (v[2] * a + v[4] * b == v[6] && v[3] * a + v[5] * b == v[7]) {
                cost = 3 * a + b
                if (best == -1 || cost < best)
                    best = cost
            }
        }
    }
    if (best != -1) {
        n++
        t += best
    }
}
END {
    print n + 0, t + 0
}