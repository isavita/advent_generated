
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    v = 0
    for (d1 = 9; d1 >= 0; d1--) {
        p = index($0, d1)
        if (p > 0 && p < length($0)) {
            max_d2 = -1
            for (j = p + 1; j <= length($0); j++) {
                c = substr($0, j, 1)
                if (c ~ /[0-9]/ && c > max_d2) max_d2 = c
            }
            if (max_d2 != -1) {
                v = d1 * 10 + max_d2
                break
            }
        }
    }
    total += v
}
END {
    printf "Total output joltage: %d\n", total
}
