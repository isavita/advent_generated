
#!/usr/bin/awk -f

function bfs(start_coord, dist,    q_head, q_tail, q, curr, parts, r, c, d, i, dr, dc, nr, nc, ncoord) {
    delete dist
    q_head = 0
    q_tail = 0

    split(start_coord, parts, ",")
    dist[start_coord] = 0
    q[q_tail++] = start_coord

    while (q_head < q_tail) {
        curr = q[q_head++]
        split(curr, parts, ",")
        r = parts[1]
        c = parts[2]
        d = dist[curr]

        for (i = 0; i < 4; i++) {
            dr = dirs[i, 0]
            dc = dirs[i, 1]
            nr = r + dr
            nc = c + dc
            ncoord = nr "," nc

            if (nr >= 0 && nr < h && nc >= 0 && nc < w) {
                if (!(ncoord in walls)) {
                    if (!(ncoord in dist)) {
                        dist[ncoord] = d + 1
                        q[q_tail++] = ncoord
                    }
                }
            }
        }
    }
}

BEGIN {
    h = 0
    w = 0
    S_coord = ""
    E_coord = ""
    
    dirs[0, 0] = 1; dirs[0, 1] = 0
    dirs[1, 0] = -1; dirs[1, 1] = 0
    dirs[2, 0] = 0; dirs[2, 1] = 1
    dirs[3, 0] = 0; dirs[3, 1] = -1

    while (getline line < "input.txt") {
        if (w == 0) {
            w = length(line)
        }
        for (c = 1; c <= w; c++) {
            char = substr(line, c, 1)
            coord = h "," (c - 1)
            grid[coord] = char
            if (char == "S") {
                S_coord = coord
            } else if (char == "E") {
                E_coord = coord
            } else if (char == "#") {
                walls[coord] = 1
            }
        }
        h++
    }
    close("input.txt")

    bfs(S_coord, dist_s)
    bfs(E_coord, dist_e)

    if (!(E_coord in dist_s)) {
        print 0
        exit
    }
    normal_cost = dist_s[E_coord]

    possible_cheats = 0
    for (r = 0; r < h; r++) {
        for (c = 0; c < w; c++) {
            start_pos = r "," c
            if (start_pos in walls) continue
            if (!(start_pos in dist_s)) continue

            sd = dist_s[start_pos]

            for (i1 = 0; i1 < 4; i1++) {
                dr1 = dirs[i1, 0]
                dc1 = dirs[i1, 1]
                m1r = r + dr1
                m1c = c + dc1

                if (m1r < 0 || m1r >= h || m1c < 0 || m1c >= w) continue

                for (i2 = 0; i2 < 4; i2++) {
                    dr2 = dirs[i2, 0]
                    dc2 = dirs[i2, 1]
                    m2r = m1r + dr2
                    m2c = m1c + dc2
                    m2_coord = m2r "," m2c

                    if (m2r < 0 || m2r >= h || m2c < 0 || m2c >= w) continue
                    if (m2_coord in walls) continue
                    if (!(m2_coord in dist_e)) continue
                    
                    ed = dist_e[m2_coord]
                    new_cost = sd + 2 + ed
                    saving = normal_cost - new_cost

                    if (saving >= 100) {
                        possible_cheats++
                    }
                }
            }
        }
    }
    print possible_cheats
}

