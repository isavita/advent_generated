
#!/usr/bin/awk -f

function pow10(n,   i, r) {
    r = 1
    for (i = 1; i <= n; i++) r *= 10
    return r
}

BEGIN {
    # read whole file, strip newlines
    while ((getline line < "input.txt") > 0) raw = raw line
    gsub(/\r|\n/, "", raw)
    split(raw, parts, /,/)

    maxEnd = 0
    for (i in parts) {
        if (parts[i] == "") continue
        split(parts[i], seg, /-/)
        start = seg[1] + 0
        end   = seg[2] + 0
        ranges[++rc] = start SUBSEP end
        if (end > maxEnd) maxEnd = end
    }

    maxK = int((length(maxEnd) + 1) / 2)

    for (k = 1; k <= maxK; k++) {
        pow10k = pow10(k)
        mult   = pow10k + 1
        minS   = (k == 1) ? 1 : pow10(k - 1)
        maxS   = pow10k - 1

        for (r = 1; r <= rc; r++) {
            split(ranges[r], se, SUBSEP)
            s = se[1]; e = se[2]

            sMin = int((s + mult - 1) / mult)
            sMax = int(e / mult)

            low  = (sMin > minS) ? sMin : minS
            high = (sMax < maxS) ? sMax : maxS

            for (cur = low; cur <= high; cur++) {
                id = cur * mult
                ids[id] = 1
            }
        }
    }

    sum = 0
    for (id in ids) sum += id + 0
    print sum
}
