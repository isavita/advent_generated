
#!/bin/bash

awk '
{
    g[NR] = $0
    s_idx = index($0, "S")
    if (s_idx > 0) {
        startY = NR
        startX = s_idx
    }
}
END {
    if (!startY) {
        print 0
        exit
    }
    counts[startX] = 1
    for (y = startY; y <= NR; y++) {
        delete nextCounts
        for (x in counts) {
            char = substr(g[y], x, 1)
            if (char == "^") {
                nextCounts[x-1] += counts[x]
                nextCounts[x+1] += counts[x]
            } else {
                nextCounts[x] += counts[x]
            }
        }
        delete counts
        for (x in nextCounts) counts[x] = nextCounts[x]
    }
    total = 0
    for (x in counts) total += counts[x]
    print total
}
' input.txt
