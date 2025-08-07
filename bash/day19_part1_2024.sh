
#!/usr/bin/env bash
# Count how many design strings can be formed from the given patterns.
# Input is read from input.txt.

awk '
BEGIN { count = 0 }

# First line: comma‑separated patterns
NR == 1 {
    # Trim leading/trailing whitespace
    gsub(/^[ \t]+|[ \t]+$/,"", $0)
    split($0, raw, ",")
    for (i = 1; i <= length(raw); i++) {
        pat = raw[i]
        gsub(/^[ \t]+|[ \t]+$/,"", pat)   # trim each pattern
        if (pat == "") next
        patterns[pat] = 1
        patlen[pat] = length(pat)
        patterns_list[++pl] = pat
    }
    next
}

# Second line is a separator – ignore it
NR == 2 { next }

# Remaining lines: design strings
{
    design = $0
    gsub(/^[ \t]+|[ \t]+$/,"", design)
    if (design == "") next
    n = length(design)
    dp[0] = 1
    for (i = 1; i <= n; i++) {
        dp[i] = 0
        for (j = 1; j <= pl; j++) {
            pat = patterns_list[j]
            lp  = patlen[pat]
            if (lp > i) continue
            if (dp[i - lp] && substr(design, i - lp + 1, lp) == pat) {
                dp[i] = 1
                break
            }
        }
    }
    if (dp[n]) count++
}

END { print count }
' input.txt
