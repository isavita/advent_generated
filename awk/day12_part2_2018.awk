BEGIN {
    while ((getline < "input.txt") > 0) {
        if ($0 ~ /initial state:/) {
            split($0, a, ": "); s = a[2]
            for (i = 1; i <= length(s); i++)
                if (substr(s, i, 1) == "#") pots[i-1] = 1
        } else if ($0 ~ /=>/) {
            split($0, a, " => "); rules[a[1]] = a[2]
        }
    }
    limit = 50000000000
    for (g = 1; g <= limit; g++) {
        min = 1e18; max = -1e18
        for (p in pots) {
            v = int(p); if (v < min) min = v; if (v > max) max = v
        }
        delete next_pots; n_min = 1e18; n_max = -1e18
        for (i = min - 2; i <= max + 2; i++) {
            pat = ""
            for (j = i - 2; j <= i + 2; j++)
                pat = pat ((j in pots) ? "#" : ".")
            if (rules[pat] == "#") {
                next_pots[i] = 1
                if (i < n_min) n_min = i; if (i > n_max) n_max = i
            }
        }
        delete pots; curr_sum = 0
        for (p in next_pots) {
            v = int(p); pots[v] = 1; curr_sum += v
        }
        curr_pattern = ""
        if (n_min != 1e18) {
            for (i = n_min; i <= n_max; i++)
                curr_pattern = curr_pattern ((i in pots) ? "#" : ".")
        }
        if (curr_pattern == prev_pattern) {
            printf "%.0f\n", curr_sum + (limit - g) * (curr_sum - prev_sum)
            exit
        }
        prev_pattern = curr_pattern; prev_sum = curr_sum
    }
    printf "%.0f\n", curr_sum
}