
#!/usr/bin/env bash
# -------------  mix_and_sum.sh  -----------------
# Reads numbers from input.txt, multiplies each by 811589153,
# performs 10 rounds of the mixing algorithm,
# and prints the sum of the values at positions
# zero+1000, zero+2000, zero+3000 (mod n).

awk '
BEGIN {
    # read all numbers, multiply by 811589153
    while ((getline line < "input.txt") > 0) {
        vals[++n] = line * 811589153
        list[n] = n          # list holds indices of vals
    }
    # perform 10 rounds of mixing
    for (round = 1; round <= 10; round++) {
        for (i = 1; i <= n; i++) {
            # find current position of element i in list
            for (pos = 1; pos <= n; pos++) if (list[pos] == i) break
            oldpos = pos - 1
            newpos = (oldpos + vals[i]) % (n - 1)
            if (newpos < 0) newpos += (n - 1)
            newpos1 = newpos + 1
            if (pos < newpos1) {
                for (j = pos; j < newpos1; j++) list[j] = list[j+1]
            } else if (pos > newpos1) {
                for (j = pos; j > newpos1; j--) list[j] = list[j-1]
            }
            list[newpos1] = i
        }
    }
    # find position of zero
    for (i = 1; i <= n; i++) if (vals[i] == 0) { zeroPos = i; break }
    for (pos = 1; pos <= n; pos++) if (list[pos] == zeroPos) break
    zeroIdx = pos
    sum = 0
    for (offset = 1000; offset <= 3000; offset += 1000) {
        idx = list[((zeroIdx - 1 + offset) % n) + 1]
        sum += vals[idx]
    }
    print sum
}
' 
