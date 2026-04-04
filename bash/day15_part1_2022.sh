
#!/bin/bash
awk '
BEGIN {
    target = 2000000
}
{
    gsub(/[^-0-9]+/, " ")
    sx = $1; sy = $2; bx = $3; by = $4
    d = (sx > bx ? sx - bx : bx - sx) + (sy > by ? sy - by : by - sy)
    rem = d - (sy > target ? sy - target : target - sy)
    if (rem >= 0) {
        n++
        s[n] = sx - rem
        e[n] = sx + rem
    }
    if (by == target) b[bx] = 1
}
END {
    for (i = 1; i <= n; i++) {
        for (j = i + 1; j <= n; j++) {
            if (s[i] > s[j]) {
                t = s[i]; s[i] = s[j]; s[j] = t
                t = e[i]; e[i] = e[j]; e[j] = t
            }
        }
    }

    total = 0
    if (n > 0) {
        cs = s[1]; ce = e[1]
        for (i = 2; i <= n; i++) {
            if (s[i] <= ce + 1) {
                if (e[i] > ce) ce = e[i]
            } else {
                total += ce - cs + 1
                cs = s[i]; ce = e[i]
            }
        }
        total += ce - cs + 1
    }

    for (x in b) {
        for (i = 1; i <= n; i++) {
            if (x >= s[i] && x <= e[i]) {
                total--
                break
            }
        }
    }

    print total
}
' input.txt
