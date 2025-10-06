#!/usr/bin/awk -f
BEGIN {
    size = 0
    while ((getline line < "input.txt") > 0) {
        gsub(/\r/, "", line)
        if (line ~ /^[[:space:]]*$/) continue
        val[size] = int(line)
        pos[size] = size
        size++
    }
    close("input.txt")

    if (size <= 0) { exit }

    n = size - 1
    for (p = 0; p < size; p++) order[p] = p

    for (i = 0; i < size; i++) {
        oldpos = pos[i]
        tmp = (oldpos + val[i]) % n
        if (tmp < 0) tmp += n
        newpos = tmp

        if (oldpos < newpos) {
            for (p = oldpos + 1; p <= newpos; p++) {
                e = order[p]
                pos[e]--
                order[p - 1] = e
            }
            order[newpos] = i
            pos[i] = newpos
        } else if (newpos < oldpos) {
            for (p = oldpos - 1; p >= newpos; p--) {
                e = order[p]
                pos[e]++
                order[p + 1] = e
            }
            order[newpos] = i
            pos[i] = newpos
        }
    }

    l = size
    zeroPos = -1
    for (i = 0; i < size; i++) {
        if (val[i] == 0) { zeroPos = pos[i]; break }
    }

    t1 = (zeroPos + 1000) % l
    t2 = (zeroPos + 2000) % l
    t3 = (zeroPos + 3000) % l

    sum = 0
    for (i = 0; i < size; i++) {
        if (pos[i] == t1 || pos[i] == t2 || pos[i] == t3) {
            sum += val[i]
        }
    }

    print sum
    exit
}