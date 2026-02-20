#!/usr/bin/awk -f
BEGIN {
    total = 0
    while ((getline line < "input.txt") > 0) {
        split(line, parts, ":")
        target = parts[1] + 0
        numsStr = parts[2]
        if (numsStr == "") next
        nCount = split(numsStr, nums, " ")
        if (nCount == 1) {
            if (nums[1] == target) total += target
            next
        }
        if (canProduce(target, nums, 2, nums[1], nCount)) total += target
    }
}
END { print total }

function digits(x) {
    return length(sprintf("%d", x))
}
function concat(value, n,   nStr, pow10, i) {
    nStr = sprintf("%d", n)
    pow10 = 1
    for (i = 1; i <= length(nStr); i++) pow10 *= 10
    return value * pow10 + n
}
function canProduce(target, nums, idx, value, nCount,   n) {
    if (idx > nCount) return (value == target) ? 1 : 0
    n = nums[idx]
    if (canProduce(target, nums, idx + 1, value + n, nCount)) return 1
    if (canProduce(target, nums, idx + 1, value * n, nCount)) return 1
    if (canProduce(target, nums, idx + 1, concat(value, n), nCount)) return 1
    return 0
}