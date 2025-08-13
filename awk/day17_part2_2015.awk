
#!/usr/bin/awk -f
BEGIN {
    # Read all container sizes from input.txt
    n = 0
    while ((getline line < "input.txt") > 0) {
        containers[++n] = line + 0
    }
    minCount = 0
    ways = 0
    findComb(1, 150, 0)
    print ways
}

function findComb(idx, target, count,    i) {
    if (target == 0) {
        if (minCount == 0 || count < minCount) {
            minCount = count
            ways = 1
        } else if (count == minCount) {
            ways++
        }
        return
    }
    if (target < 0 || idx > n) return
    if (minCount > 0 && count >= minCount) return
    findComb(idx + 1, target - containers[idx], count + 1)
    findComb(idx + 1, target, count)
}
