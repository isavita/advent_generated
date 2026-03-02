
function mmin(a, b) { return a < b ? a : b }
function mmax(a, b) { return a > b ? a : b }

function count(n, x1, x2, m1, m2, a1, a2, s1, s2,    i, c, o, v, d, res, tx1, tx2, tm1, tm2, ta1, ta2, ts1, ts2) {
    if (n == "A") return (x2 - x1 + 1) * (m2 - m1 + 1) * (a2 - a1 + 1) * (s2 - s1 + 1)
    if (n == "R") return 0
    res = 0
    for (i = 1; i <= num_rules[n]; i++) {
        c = cat[n, i]; o = op[n, i]; v = val[n, i]; d = dest[n, i]
        tx1 = x1; tx2 = x2; tm1 = m1; tm2 = m2; ta1 = a1; ta2 = a2; ts1 = s1; ts2 = s2
        if (o == "<") {
            if (c == "x") { tx2 = mmin(x2, v - 1); x1 = mmax(x1, v) }
            else if (c == "m") { tm2 = mmin(m2, v - 1); m1 = mmax(m1, v) }
            else if (c == "a") { ta2 = mmin(a2, v - 1); a1 = mmax(a1, v) }
            else if (c == "s") { ts2 = mmin(s2, v - 1); s1 = mmax(s1, v) }
            if (tx1 <= tx2 && tm1 <= tm2 && ta1 <= ta2 && ts1 <= ts2) res += count(d, tx1, tx2, tm1, tm2, ta1, ta2, ts1, ts2)
        } else if (o == ">") {
            if (c == "x") { tx1 = mmax(x1, v + 1); x2 = mmin(x2, v) }
            else if (c == "m") { tm1 = mmax(m1, v + 1); m2 = mmin(m2, v) }
            else if (c == "a") { ta1 = mmax(a1, v + 1); a2 = mmin(a2, v) }
            else if (c == "s") { ts1 = mmax(s1, v + 1); s2 = mmin(s2, v) }
            if (tx1 <= tx2 && tm1 <= tm2 && ta1 <= ta2 && ts1 <= ts2) res += count(d, tx1, tx2, tm1, tm2, ta1, ta2, ts1, ts2)
        } else {
            res += count(d, x1, x2, m1, m2, a1, a2, s1, s2)
        }
        if (x1 > x2 || m1 > m2 || a1 > a2 || s1 > s2) break
    }
    return res
}

BEGIN {
    FS = "[{,}]"
    ARGV[1] = "input.txt"
    ARGC = 2
}

/^[a-z].*\{/ {
    name = $1
    num_rules[name] = NF - 2
    for (i = 2; i < NF; i++) {
        if ($i ~ /:/) {
            split($i, b, ":")
            dest[name, i - 1] = b[2]
            cat[name, i - 1] = substr(b[1], 1, 1)
            op[name, i - 1] = substr(b[1], 2, 1)
            val[name, i - 1] = substr(b[1], 3) + 0
        } else {
            dest[name, i - 1] = $i
            op[name, i - 1] = ""
        }
    }
}

END {
    printf "%.0f\n", count("in", 1, 4000, 1, 4000, 1, 4000, 1, 4000)
}
