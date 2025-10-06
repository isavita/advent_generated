#!/usr/bin/awk -f
BEGIN {
    n = 0
    while ((getline line < "input.txt") > 0) {
        if (line ~ /^[[:space:]]*$/) continue
        split(line, f, /[[:space:]]+/)
        left = f[1] + 0
        right = f[2] + 0
        L[n] = left
        n++
        CNT[right]++
    }
}
END {
    sum = 0
    for (i = 0; i < n; i++) {
        sum += L[i] * CNT[ L[i] ]
    }
    printf("Similarity score: %d\n", sum)
}