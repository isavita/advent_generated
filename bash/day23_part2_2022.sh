
#!/bin/bash
awk '
BEGIN {
    split("-1 -1 0 1 1 1 0 -1", dx)
    split("0 1 1 1 0 -1 -1 -1", dy)
    order[0] = 1 # N
    order[1] = 5 # S
    order[2] = 7 # W
    order[3] = 3 # E
    
    r = 0
    while ((getline < "input.txt") > 0) {
        for (c = 1; c <= length($0); c++) {
            if (substr($0, c, 1) == "#") map[r, c-1] = 1
        }
        r++
    }

    for (round = 0; ; round++) {
        delete prop
        delete count
        moved = 0

        for (pos in map) {
            split(pos, coord, SUBSEP)
            y = coord[1]; x = coord[2]
            
            has_neighbor = 0
            for (i = 1; i <= 8; i++) {
                if ((y + dx[i], x + dy[i]) in map) {
                    has_neighbor = 1
                    break
                }
            }
            if (!has_neighbor) continue

            for (i = 0; i < 4; i++) {
                dir = order[(round + i) % 4]
                can_move = 1
                for (k = -1; k <= 1; k++) {
                    check_idx = (dir + k - 1 + 8) % 8 + 1
                    if ((y + dx[check_idx], x + dy[check_idx]) in map) {
                        can_move = 0
                        break
                    }
                }
                if (can_move) {
                    dest = (y + dx[dir]) SUBSEP (x + dy[dir])
                    prop[pos] = dest
                    count[dest]++
                    break
                }
            }
        }

        for (pos in prop) {
            dest = prop[pos]
            if (count[dest] == 1) {
                delete map[pos]
                map[dest] = 1
                moved = 1
            }
        }

        if (!moved) {
            print round + 1
            exit
        }
    }
}'
