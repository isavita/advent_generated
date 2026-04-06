
#!/bin/bash
awk '
/^initial state:/ {
    s = $3
    for (i = 1; i <= length(s); i++) if (substr(s, i, 1) == "#") pots[i-1] = 1
}
/=>/ { rules[$1] = $3 }
END {
    limit = 50000000000
    for (g = 1; g <= limit; g++) {
        min = 1e15; max = -1e15
        for (p in pots) {
            v = int(p)
            if (v < min) min = v
            if (v > max) max = v
        }
        delete nxt; n_min = 1e15; n_max = -1e15; curr_sum = 0
        for (i = min - 2; i <= max + 2; i++) {
            pat = ""
            for (j = i - 2; j <= i + 2; j++) pat = pat ((j in pots) ? "#" : ".")
            if (rules[pat] == "#") {
                nxt[i] = 1; curr_sum += i
                if (i < n_min) n_min = i; if (i > n_max) n_max = i
            }
        }
        curr_pat = ""
        for (i = n_min; i <= n_max; i++) curr_pat = curr_pat ((i in nxt) ? "#" : ".")
        if (curr_pat == prev_pat) {
            printf "%.0f\n", curr_sum + (limit - g) * (curr_sum - prev_sum)
            exit
        }
        delete pots; for (p in nxt) pots[p] = 1
        prev_pat = curr_pat; prev_sum = curr_sum
    }
    printf "%.0f\n", curr_sum
}' input.txt
