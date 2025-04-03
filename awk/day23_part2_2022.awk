
#!/usr/bin/awk -f

BEGIN {
    SUBSEP = ","
    # N(1), S(5), W(7), E(3) relative to dirs indices
    order[0] = 1
    order[1] = 5
    order[2] = 7
    order[3] = 3

    # All 8 directions dx/dy (index 0..7)
    dx[0]=-1; dy[0]=-1
    dx[1]=-1; dy[1]= 0
    dx[2]=-1; dy[2]= 1
    dx[3]= 0; dy[3]= 1
    dx[4]= 1; dy[4]= 1
    dx[5]= 1; dy[5]= 0
    dx[6]= 1; dy[6]=-1
    dx[7]= 0; dy[7]=-1

    curr_dir_idx = 0
    row = 0
    while (getline line < "input.txt" > 0) {
        len = length(line)
        for (col = 0; col < len; ++col) {
            if (substr(line, col + 1, 1) == "#") {
                map[row, col] = 1
            }
        }
        row++
    }
    close("input.txt")

    main()
}

function main() {
    round = 0
    while (1) {
        delete proposes
        delete next_pos
        someone_moved = 0

        # Proposal Phase
        for (pos in map) {
            split(pos, coords, SUBSEP); r = coords[1]; c = coords[2]

            alone = 1
            for (i = 0; i < 8; ++i) {
                if ((r + dx[i], c + dy[i]) in map) {
                    alone = 0
                    break
                }
            }
            if (alone) continue

            proposed_this_elf = 0
            for (j = 0; j < 4; ++j) {
                dir_idx = order[(curr_dir_idx + j) % 4]
                can_propose = 1
                for (k = -1; k <= 1; ++k) {
                     check_idx = (dir_idx + k + 8) % 8
                     check_r = r + dx[check_idx]
                     check_c = c + dy[check_idx]
                     if ((check_r, check_c) in map) {
                         can_propose = 0
                         break
                     }
                }

                if (can_propose) {
                    dest_r = r + dx[dir_idx]
                    dest_c = c + dy[dir_idx]
                    dest = dest_r SUBSEP dest_c
                    next_pos[pos] = dest
                    proposes[dest]++
                    proposed_this_elf = 1
                    break
                }
            }
        }

        # Movement Phase
        for (pos in next_pos) {
            dest = next_pos[pos]
            if (proposes[dest] == 1) {
                delete map[pos]
                map[dest] = 1
                someone_moved = 1
            }
        }

        if (!someone_moved) {
            print round + 1
            exit
        }

        round++
        curr_dir_idx = (curr_dir_idx + 1) % 4
    }
}

