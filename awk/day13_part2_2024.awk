BEGIN {
    RS = ""
    ARGV[1] = "input.txt"
    ARGC = 2
    off = 10000000000000
}
{
    gsub(/[^-0-9]+/, " ")
    if (NF < 6) next
    ax = $1; ay = $2; bx = $3; by = $4; px = $5 + off; py = $6 + off
    d = ax * by - ay * bx
    if (d != 0) {
        na = px * by - py * bx
        nb = py * ax - px * ay
        if (na % d == 0 && nb % d == 0) {
            a = na / d; b = nb / d
            if (a >= 0 && b >= 0) {
                s++
                t += 3 * a + b
            }
        }
    }
}
END {
    printf "%d %.0f\n", s, t
}