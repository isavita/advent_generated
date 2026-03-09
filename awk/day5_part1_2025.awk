
#!/usr/bin/awk -f
BEGIN {
    parsing = 1
    n = 0
    fresh = 0
}
END {
    while ((getline line < "input.txt") > 0) {
        if (line ~ /^[[:space:]]*$/) {
            parsing = 0
            continue
        }
        if (parsing) {
            split(line, a, "-")
            ++n
            mins[n] = a[1] + 0
            maxs[n] = a[2] + 0
        } else {
            id = line + 0
            for (i = 1; i <= n; i++) {
                if (id >= mins[i] && id <= maxs[i]) {
                    ++fresh
                    break
                }
            }
        }
    }
    close("input.txt")
    printf "Number of fresh ingredients: %d\n", fresh
}
