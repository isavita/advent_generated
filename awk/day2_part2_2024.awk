
#!/usr/bin/awk -f

# absolute value
function abs(x) { return (x < 0) ? -x : x }

# check if a sequence is safe
function isSafe(arr, n,    i, diff, firstDiff, inc) {
    if (n < 2) return 0

    firstDiff = arr[2] - arr[1]
    if (firstDiff == 0) return 0
    inc = (firstDiff > 0)

    for (i = 1; i < n; i++) {
        diff = arr[i+1] - arr[i]
        if (diff == 0) return 0
        if ((inc && diff <= 0) || (!inc && diff >= 0)) return 0
        if (abs(diff) < 1 || abs(diff) > 3) return 0
    }
    return 1
}

# check if removing exactly one element makes the sequence safe
function isSafeWithOneRemoval(arr, n,    i, j, k, tmp, tmpLen) {
    if (n <= 2) return 0

    for (i = 1; i <= n; i++) {
        tmpLen = 0
        for (j = 1; j <= n; j++) {
            if (j == i) continue
            tmp[++tmpLen] = arr[j]
        }
        if (isSafe(tmp, tmpLen)) return 1
    }
    return 0
}

BEGIN {
    safeCount = 0
    while ((getline line < "input.txt") > 0) {
        n = split(line, levels, /[ \t\r\n]+/)
        if (isSafe(levels, n) || isSafeWithOneRemoval(levels, n))
            ++safeCount
    }
    print safeCount
    exit
}
