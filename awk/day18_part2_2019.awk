
#!/usr/bin/awk -f

function c2b(c) {
    return index("abcdefghijklmnopqrstuvwxyz", c) - 1
}

function my_tolower(c, i) {
    i = index("ABCDEFGHIJKLMNOPQRSTUVWXYZ", c)
    return i ? substr("abcdefghijklmnopqrstuvwxyz", i, 1) : c
}

function has_bit(mask, bit) {
    return int(mask / p2[bit]) % 2 == 1
}

function set_bit(m, b) {
    if (int(m / p2[b]) % 2 == 1) return m
    return m + p2[b]
}

function can_pass(rq, gt, b) {
    if (rq == 0) return 1
    for (b = 0; b < 26; b++) {
        if (p2[b] > rq) break
        if (int(rq / p2[b]) % 2 == 1 && int(gt / p2[b]) % 2 == 0) return 0
    }
    return 1
}

BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
    for (i = 0; i <= 30; i++) p2[i] = 2^i
    split("0,1,0,-1,1,0,-1,0", dxy, ",")
}

{
    line[NR] = $0
    width = length($0)
    for (x = 1; x <= width; x++) g[NR, x] = substr($0, x, 1)
}

END {
    height = NR
    found = 0
    for (y = 2; y < height; y++) {
        for (x = 2; x < width; x++) {
            if (g[y, x] == "@" && g[y-1, x] == "." && g[y+1, x] == "." && g[y, x-1] == "." && g[y, x+1] == ".") {
                g[y-1, x-1]="@"; g[y-1, x]="#"; g[y-1, x+1]="@"
                g[y, x-1]="#";   g[y, x]="#";   g[y, x+1]="#"
                g[y+1, x-1]="@"; g[y+1, x]="#"; g[y+1, x+1]="@"
                found = 1; break
            }
        }
        if (found) break
    }

    id_cnt = 0
    for (y = 1; y <= height; y++) {
        for (x = 1; x <= width; x++) {
            if (g[y, x] == "@") {
                id_cnt++
                id_x[id_cnt] = x; id_y[id_cnt] = y
            }
        }
    }
    num_robots = id_cnt
    for (y = 1; y <= height; y++) {
        for (x = 1; x <= width; x++) {
            if (g[y, x] ~ /[a-z]/) {
                id_cnt++
                id_x[id_cnt] = x; id_y[id_cnt] = y
                char_to_id[g[y, x]] = id_cnt
                bit = c2b(g[y, x])
                key_to_bit[id_cnt] = bit
                if (!has_bit(target_mask, bit)) target_mask += p2[bit]
            }
        }
    }

    for (i = 1; i <= id_cnt; i++) {
        delete v; delete q_bfs
        q_bfs[1] = id_x[i]; q_bfs[2] = id_y[i]; q_bfs[3] = 0; q_bfs[4] = 0
        v[id_x[i], id_y[i]] = 1
        h = 1; t = 4
        while (h <= t) {
            cx = q_bfs[h++]; cy = q_bfs[h++]; cd = q_bfs[h++]; cm = q_bfs[h++]
            char = g[cy, cx]
            nm_base = cm
            if (char ~ /[a-z]/ && i != char_to_id[char]) {
                tid = char_to_id[char]
                adj[i, tid] = cd; req[i, tid] = cm
                if (i <= num_robots) key_robot[tid] = i
                nm_base = set_bit(cm, c2b(char))
            }
            for (d = 1; d <= 7; d += 2) {
                nx = cx + dxy[d]; ny = cy + dxy[d+1]
                if (nx < 1 || nx > width || ny < 1 || ny > height) continue
                nc = g[ny, nx]
                if (nc != "#" && !((nx, ny) in v)) {
                    v[nx, ny] = 1; nm = nm_base
                    if (nc ~ /[A-Z]/) nm = set_bit(nm, c2b(my_tolower(nc)))
                    q_bfs[++t] = nx; q_bfs[++t] = ny; q_bfs[++t] = cd+1; q_bfs[++t] = nm
                }
            }
        }
    }

    st = ""
    for (i = 1; i <= 4; i++) st = st (i <= num_robots ? i : 0) ","
    st = st "0"
    dists[st] = 0; q[0, 1] = st; q_p[0] = 1; max_c = 0
    for (c = 0; c <= max_c; c++) {
        if (!q_p[c]) continue
        for (qi = 1; qi <= q_p[c]; qi++) {
            s = q[c, qi]; delete q[c, qi]
            if (dists[s] < c) continue
            split(s, p, ",")
            mask = p[5]
            if (mask + 0 == target_mask) { print c; exit }
            for (r = 1; r <= 4; r++) {
                fid = p[r]; if (fid == 0) continue
                for (tid = num_robots + 1; tid <= id_cnt; tid++) {
                    if (key_robot[tid] != r || has_bit(mask, key_to_bit[tid])) continue
                    if (adj[fid, tid] && can_pass(req[fid, tid], mask)) {
                        nm = mask + p2[key_to_bit[tid]]
                        ns = (r==1?tid:p[1]) "," (r==2?tid:p[2]) "," (r==3?tid:p[3]) "," (r==4?tid:p[4]) "," nm
                        nc = c + adj[fid, tid]
                        if (!(ns in dists) || nc < dists[ns]) {
                            dists[ns] = nc; q[nc, ++q_p[nc]] = ns
                            if (nc > max_c) max_c = nc
                        }
                    }
                }
            }
        }
        delete q_p[c]
    }
}
