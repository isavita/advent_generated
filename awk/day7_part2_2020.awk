
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    p = $1 " " $2
    for (i = 5; i <= NF; i++) {
        if ($i ~ /^[0-9]+$/) {
            rules[p] = rules[p] $i "," $(i+1) " " $(i+2) "|"
        }
    }
}
function c(b, a, i, n, r, s) {
    if (b in m) return m[b]
    s = 1
    n = split(rules[b], a, "|")
    for (i = 1; i <= n; i++) {
        if (a[i]) {
            split(a[i], r, ",")
            s += r[1] * c(r[2])
        }
    }
    return m[b] = s
}
END {
    print c("shiny gold") - 1
}
