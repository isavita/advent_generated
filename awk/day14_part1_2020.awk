
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
$1 == "mask" {
    m = $3
    next
}
{
    split($1, a, "[][]")
    v = $3; r = 0; p = 1
    for (i = 36; i > 0; i--) {
        c = substr(m, i, 1)
        r += (c == "X" ? int(v / p) % 2 : c) * p
        p *= 2
    }
    mem[a[2]] = r
}
END {
    for (k in mem) s += mem[k]
    printf "%.0f\n", s
}
