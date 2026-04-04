
#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN {
    while ((getline line < "input.txt") > 0) content = content line
    gsub(/\r|\n/, "", content)
    n = split(content, parts, ",")
    for (i = 1; i <= n; i++) {
        p = parts[i]
        if (p == "") continue
        split(p, b, "-")
        rstart = b[1] + 0
        rend = b[2] + 0
        sLen = length(b[1])
        eLen = length(b[2])

        for (totalLen = sLen; totalLen <= eLen; totalLen++) {
            half = int(totalLen / 2)
            for (k = 1; k <= half; k++) {
                if (totalLen % k != 0) continue
                reps = totalLen / k
                M = 0
                pow = 1
                for (j = 0; j < reps; j++) {
                    M += pow
                    pow *= 10 ^ k
                }
                minSeed = 10 ^ (k - 1)
                maxSeed = 10 ^ k - 1
                targetMin = int((rstart + M - 1) / M)
                targetMax = int(rend / M)
                start = (targetMin > minSeed) ? targetMin : minSeed
                end = (targetMax < maxSeed) ? targetMax : maxSeed
                if (start > end) continue
                for (seed = start; seed <= end; seed++) found[seed * M] = 1
            }
        }
    }

    sum = 0
    for (v in found) sum += v + 0
    print sum
}
' 
