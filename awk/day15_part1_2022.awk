
#!/usr/bin/awk -f
BEGIN{
    target = 2000000          # y‑line to inspect
    n = 0                     # number of interval entries

    # ----- read input.txt -------------------------------------------------
    while ((getline line < "input.txt") > 0) {
        # keep only numbers (including possible minus sign)
        gsub(/[^-0-9]+/, " ", line)
        split(line, v)

        sx = v[1];  sy = v[2]          # sensor position
        bx = v[3];  by = v[4]          # closest beacon

        # Manhattan distance sensor‑beacon
        dist = (sx > bx ? sx - bx : bx - sx) + (sy > by ? sy - by : by - sy)

        # horizontal reach on the required y line
        rem = dist - (sy > target ? sy - target : target - sy)
        if (rem >= 0) {
            ++n
            start[n] = sx - rem
            end[n]   = sx + rem
        }

        # remember beacons that sit exactly on the target line
        if (by == target)
            beacons[bx] = 1
    }

    # ----- sort intervals by start (simple bubble sort, sensors are few) --
    for (i = 1; i <= n; i++) {
        for (j = i + 1; j <= n; j++) {
            if (start[i] > start[j]) {
                tmp = start[i]; start[i] = start[j]; start[j] = tmp
                tmp = end[i];   end[i]   = end[j];   end[j]   = tmp
            }
        }
    }

    # ----- merge intervals and count covered points -----------------------
    total = 0
    if (n > 0) {
        cur_s = start[1]; cur_e = end[1]
        for (i = 2; i <= n; i++) {
            if (start[i] <= cur_e + 1) {
                if (end[i] > cur_e) cur_e = end[i]
            } else {
                total += cur_e - cur_s + 1
                cur_s = start[i]; cur_e = end[i]
            }
        }
        total += cur_e - cur_s + 1
    }

    # ----- subtract beacons that lie on the target line --------------------
    for (bx in beacons) {
        for (i = 1; i <= n; i++) {
            if (bx >= start[i] && bx <= end[i]) {
                total--
                break
            }
        }
    }

    print total
}
