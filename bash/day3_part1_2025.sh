
#!/usr/bin/env bash
awk '
BEGIN { total = 0 }
{
    s = $0
    v = 0
    for (d = 9; d >= 0; d--) {
        p = index(s, d)
        if (p > 0 && p < length(s)) {
            max = -1
            for (i = p + 1; i <= length(s); i++) {
                c = substr(s, i, 1)
                if (c ~ /[0-9]/ && c > max) max = c
            }
            if (max != -1) {
                v = d * 10 + max
                break
            }
        }
    }
    total += v
}
END {
    printf "Total output joltage: %d\n", total
}
' input.txt
