#!/usr/bin/awk -f
function calculateWaysToWinLongRace(time, record,   low, high, mid, dist, firstWin, lastWin) {
    low = 0
    high = time
    while (low <= high) {
        mid = int(low + (high - low) / 2)
        dist = mid * (time - mid)
        if (dist > record) {
            high = mid - 1
        } else {
            low = mid + 1
        }
    }
    firstWin = low

    low = 0
    high = time
    while (low <= high) {
        mid = int(low + (high - low) / 2)
        dist = mid * (time - mid)
        if (dist > record) {
            low = mid + 1
        } else {
            high = mid - 1
        }
    }
    lastWin = high

    return (lastWin - firstWin + 1)
}
BEGIN {
    fname = "input.txt"
    t = 0
    d = 0
    while ((getline line < fname) > 0) {
        if (line == "") continue
        pos = index(line, ":")
        if (pos > 0) {
            values = substr(line, pos + 1)
            gsub(/ /, "", values)
            if (t == 0) {
                t = values + 0
            } else {
                d = values + 0
            }
        }
    }
    close(fname)
    ans = calculateWaysToWinLongRace(t, d)
    print ans
    exit
}