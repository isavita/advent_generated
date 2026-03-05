
BEGIN {
    ARGC = 2
    ARGV[1] = "input.txt"
}
{
    p[++n] = $4
    s[n] = $12
}
END {
    p[++n] = 11
    s[n] = 0
    t = 0
    step = 1
    for (i = 1; i <= n; i++) {
        while ((s[i] + t + i) % p[i]) {
            t += step
        }
        step *= p[i]
    }
    print t
}
