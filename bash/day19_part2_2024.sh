
#!/usr/bin/env bash
#  solve.sh – reads input.txt and prints the total number of ways

awk '
BEGIN {
    # read the first line – the comma‑separated patterns
    getline patterns_line < "input.txt"
    n = split(patterns_line, raw, ",")
    for (i = 1; i <= n; i++) {
        pat = raw[i]
        gsub(/^[ \t]+|[ \t]+$/,"",pat)          # trim spaces
        if (pat != "") {
            patterns[++np] = pat
            plen[np] = length(pat)
        }
    }

    # skip the separator line (if any)
    getline < "input.txt"

    total = 0
}

{
    design = $0
    gsub(/^[ \t]+|[ \t]+$/,"",design)          # trim spaces
    if (length(design) == 0) next

    n = length(design)
    dp[0] = 1
    for (i = 1; i <= n; i++) dp[i] = 0

    for (i = 1; i <= n; i++) {
        for (p = 1; p <= np; p++) {
            lp = plen[p]
            if (lp > 0 && i >= lp) {
                if (substr(design, i-lp+1, lp) == patterns[p]) {
                    dp[i] += dp[i-lp]
                }
            }
        }
    }
    total += dp[n]
}

END {
    print total
}
' input.txt
