#!/usr/bin/awk -f
function process(line, nums, n, firstDiff, isInc, i, diff, absDiff, ok) {
    n = split(line, nums, /[[:space:]]+/)
    if (n < 2) return
    firstDiff = nums[2] - nums[1]
    if (firstDiff == 0) return
    isInc = firstDiff > 0
    ok = 1
    for (i = 1; i <= n-1; i++) {
        diff = nums[i+1] - nums[i]
        if (diff == 0) { ok = 0; break }
        if ((isInc && diff <= 0) || (!isInc && diff >= 0)) { ok = 0; break }
        absDiff = diff < 0 ? -diff : diff
        if (absDiff < 1 || absDiff > 3) { ok = 0; break }
    }
    if (ok) count++
}
BEGIN {
    count = 0
    while ((getline line < "input.txt") > 0) {
        process(line)
    }
    print count
    exit
}