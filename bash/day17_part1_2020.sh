
awk '
BEGIN {
    FS = ""
    y = 0
    while ((getline < "input.txt") > 0) {
        for (x = 1; x <= NF; x++)
            if ($x == "#") active[x "," y ",0"] = 1
        y++
    }
    for (i = 0; i < 6; i++) {
        delete neighbors
        for (pos in active) {
            split(pos, c, ",")
            for (dx = -1; dx <= 1; dx++)
                for (dy = -1; dy <= 1; dy++)
                    for (dz = -1; dz <= 1; dz++)
                        if (dx || dy || dz)
                            neighbors[(c[1]+dx) "," (c[2]+dy) "," (c[3]+dz)]++
        }
        delete next_active
        for (pos in neighbors) {
            cnt = neighbors[pos]
            if (cnt == 3 || (cnt == 2 && pos in active))
                next_active[pos] = 1
        }
        delete active
        for (pos in next_active) active[pos] = 1
    }
    for (pos in active) count++
    print count
}'
