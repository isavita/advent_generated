#!/usr/bin/awk -f
BEGIN {
    fname = "input.txt"
    total = 0
    pc = 0
    if ((getline line < fname) <= 0) { print 0; exit }
    gsub(/\r$/, "", line)
    n = split(line, tmp, ",")
    for (i = 1; i <= n; i++) {
        t = trim(tmp[i])
        if (t != "") pat[++pc] = t
    }
    if ((getline dummy < fname) > 0) { }
    while ((getline line < fname) > 0) {
        line = trim(line)
        if (line != "") total += count_ways(line, pc)
    }
    close(fname)
    print total
    exit
}
function trim(s) {
    gsub(/^[ \t\r\n]+/, "", s)
    gsub(/[ \t\r\n]+$/, "", s)
    return s
}
function count_ways(design, pc) {
    n = length(design)
    if (n == 0) return 0
    for (i = 0; i <= n; i++) dp[i] = 0
    dp[0] = 1
    for (i = 1; i <= n; i++) {
        dp[i] = 0
        for (k = 1; k <= pc; k++) {
            p = pat[k]
            lp = length(p)
            if (lp > 0 && i >= lp) {
                if (substr(design, i - lp + 1, lp) == p) {
                    dp[i] += dp[i - lp]
                }
            }
        }
    }
    return dp[n]
}