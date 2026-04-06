
#!/bin/bash

awk '
{
    h = NR
    w = split($0, chars, "")
    for (i = 1; i <= w; i++) g[h, i] = chars[i]
}
END {
    while (1) {
        changed = 0
        for (i = 1; i <= h; i++) {
            for (j = 1; j <= w; j++) {
                prev[i, j] = g[i, j]
            }
        }
        for (i = 1; i <= h; i++) {
            for (j = 1; j <= w; j++) {
                curr = prev[i, j]
                if (curr == ".") continue
                n = 0
                for (y = i - 1; y <= i + 1; y++) {
                    for (x = j - 1; x <= j + 1; x++) {
                        if ((y != i || x != j) && prev[y, x] == "#") n++
                    }
                }
                if (curr == "L" && n == 0) {
                    g[i, j] = "#"
                    changed = 1
                } else if (curr == "#" && n >= 4) {
                    g[i, j] = "L"
                    changed = 1
                }
            }
        }
        if (!changed) break
    }
    for (i = 1; i <= h; i++) {
        for (j = 1; j <= w; j++) {
            if (g[i, j] == "#") count++
        }
    }
    print count
}' input.txt
