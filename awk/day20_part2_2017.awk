
#!/usr/bin/awk -f
# reads input.txt, simulates 1000 ticks and removes colliding particles

BEGIN {
    # load the file
    while ((getline line < "input.txt") > 0) {
        # each line: p=<x,y,z>, v=<x,y,z>, a=<x,y,z>
        split(line, parts, /, /)               # three parts: p=…, v=…, a=…
        id = ++n                                # particle id (1‑based)

        for (i = 1; i <= 3; i++) {
            # strip the leading "p=<", "v=<", "a=<" and trailing ">"
            sub(/^.[=<]/, "", parts[i])
            sub(/>$/, "", parts[i])
            split(parts[i], coords, ",")
            for (j = 1; j <= 3; j++) {
                val = coords[j] + 0
                if (i == 1)   p[id, j-1] = val          # position
                else if (i == 2) v[id, j-1] = val       # velocity
                else            a[id, j-1] = val       # acceleration
            }
        }
        ids[id] = id            # keep list of active ids
    }
    close("input.txt")

    # simulation: 1000 ticks
    for (tick = 1; tick <= 1000; tick++) {
        delete cnt               # position → count

        # 1) update velocity & position, record positions
        for (idx = 1; idx <= n; idx++) {
            id = ids[idx]
            if (id == "") continue

            for (d = 0; d < 3; d++) {
                v[id,d] += a[id,d]
                p[id,d] += v[id,d]
            }
            pos = p[id,0] SUBSEP p[id,1] SUBSEP p[id,2]
            cnt[pos]++
            pos_of[id] = pos
        }

        # 2) keep only particles whose position is unique
        m = 0
        delete newids
        for (idx = 1; idx <= n; idx++) {
            id = ids[idx]
            if (id == "") continue
            if (cnt[pos_of[id]] == 1) {
                newids[++m] = id
            } else {
                # clear data of removed particles (optional, frees memory)
                for (d = 0; d < 3; d++) {
                    delete p[id,d]
                    delete v[id,d]
                    delete a[id,d]
                }
                delete pos_of[id]
            }
        }
        # replace old id list with survivors
        delete ids
        for (i = 1; i <= m; i++) ids[i] = newids[i]
        n = m
    }

    print n
}
