
#!/bin/bash
awk '
function can_move(dx, dy,    i, nx, ny) {
    for (i = 0; i < n_pts; i++) {
        nx = rx[i] + dx; ny = ry[i] + dy
        if (nx < 0 || nx > 6 || ny < 1 || ((nx SUBSEP ny) in chamber)) return 0
    }
    return 1
}
function get_profile(    x, y, p, h) {
    p = ""
    for (x = 0; x < 7; x++) {
        h = 0
        for (y = max_y; y > max_y - 30; y--) {
            if ((x SUBSEP y) in chamber) { h = max_y - y; break }
            if (y <= 0) { h = max_y; break }
        }
        p = p h ","
    }
    return p
}
BEGIN {
    getline jet < "input.txt"
    len_j = length(jet)
    split("4,5,5,4,4", s_len, ",")
    split("0,1,2,3, 1,0,1,2,1, 0,1,2,2,2, 0,0,0,0, 0,1,0,1", s_dx, ",")
    split("0,0,0,0, 0,1,1,1,2, 0,0,0,1,2, 0,1,2,3, 0,0,1,1", s_dy, ",")
    s_off[0]=1; s_off[1]=5; s_off[2]=10; s_off[3]=15; s_off[4]=19
    max_y = 0; rock_n = 0; jet_i = 0; add_h = 0; target = 1000000000000
    while (rock_n < target) {
        rt = rock_n % 5; n_pts = s_len[rt+1]; off = s_off[rt]
        for (i=0; i<n_pts; i++) { rx[i] = 2 + s_dx[off+i]; ry[i] = max_y + 4 + s_dy[off+i] }
        while (1) {
            ji = jet_i % len_j; d = substr(jet, ji + 1, 1) == ">" ? 1 : -1; jet_i++
            if (can_move(d, 0)) for (i=0; i<n_pts; i++) rx[i] += d
            if (can_move(0, -1)) for (i=0; i<n_pts; i++) ry[i] -= 1
            else {
                for (i=0; i<n_pts; i++) {
                    chamber[rx[i] SUBSEP ry[i]] = 1
                    if (ry[i] > max_y) max_y = ry[i]
                }
                break
            }
        }
        state = rt SUBSEP (jet_i % len_j) SUBSEP get_profile()
        if (state in seen) {
            split(seen[state], v, ",")
            old_n = v[1]; old_y = v[2]
            cycle_n = rock_n - old_n; cycle_h = max_y - old_y
            num_cycles = int((target - 1 - rock_n) / cycle_n)
            add_h += num_cycles * cycle_h
            rock_n += num_cycles * cycle_n
            delete seen
        }
        seen[state] = rock_n "," max_y
        rock_n++
    }
    printf "%.0f\n", max_y + add_h
}'
