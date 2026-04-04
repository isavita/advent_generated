
#!/usr/bin/env bash
awk '
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
    dx[1]=0; dy[1]=-1; dx[2]=-1; dy[2]=0; dx[3]=1; dy[3]=0; dx[4]=0; dy[4]=1
}
{
    for (x=1; x<=length($0); x++) {
        c = substr($0, x, 1)
        if (c == "E" || c == "G") {
            u_cnt++
            u_x[u_cnt] = x
            u_y[u_cnt] = NR
            u_type[u_cnt] = c
            u_hp[u_cnt] = 200
            u_alive[u_cnt] = 1
            unit_at[NR, x] = u_cnt
            grid[NR, x] = "."
        } else grid[NR, x] = c
    }
}
function do_bfs(sx, sy,    h, t, x, y, nx, ny, i, d) {
    tag++
    h = t = 1
    qx[1] = sx
    qy[1] = sy
    dist[sy, sx] = 0
    v_tag[sy, sx] = tag
    while (h <= t) {
        x = qx[h]
        y = qy[h++]
        d = dist[y, x]
        for (i=1; i<=4; i++) {
            nx = x + dx[i]
            ny = y + dy[i]
            if (grid[ny, nx] == "." && unit_at[ny, nx] == 0 && v_tag[ny, nx] != tag) {
                v_tag[ny, nx] = tag
                dist[ny, nx] = d + 1
                qx[++t] = nx
                qy[t] = ny
            }
        }
    }
}
END {
    rounds = 0
    while (1) {
        n = 0
        for (j=1; j<=u_cnt; j++) if (u_alive[j]) order[++n] = j
        for (m=1; m<=n; m++) {
            for (k=m+1; k<=n; k++) {
                if (u_y[order[k]] < u_y[order[m]] || (u_y[order[k]] == u_y[order[m]] && u_x[order[k]] < u_x[order[m]])) {
                    tmp = order[m]
                    order[m] = order[k]
                    order[k] = tmp
                }
            }
        }
        round_done = 1
        for (i=1; i<=n; i++) {
            id = order[i]
            if (!u_alive[id]) continue
            eg = 0
            gg = 0
            for (k=1; k<=u_cnt; k++) if (u_alive[k]) { if (u_type[k] == "E") eg = 1; else gg = 1 }
            if ((u_type[id] == "E" && !gg) || (u_type[id] == "G" && !eg)) { round_done = 0; break }
            adj = 0
            for (j=1; j<=4; j++) {
                tid = unit_at[u_y[id]+dy[j], u_x[id]+dx[j]]
                if (tid > 0 && u_type[tid] != u_type[id]) adj = 1
            }
            if (!adj) {
                do_bfs(u_x[id], u_y[id])
                min_d = 999999
                tx = -1
                ty = -1
                for (k=1; k<=u_cnt; k++) {
                    if (u_alive[k] && u_type[k] != u_type[id]) {
                        for (j=1; j<=4; j++) {
                            nx = u_x[k]+dx[j]
                            ny = u_y[k]+dy[j]
                            if (grid[ny, nx] == "." && unit_at[ny, nx] == 0 && v_tag[ny, nx] == tag) {
                                if (dist[ny, nx] < min_d) { min_d = dist[ny, nx]; tx = nx; ty = ny }
                                else if (dist[ny, nx] == min_d) { if (ny < ty || (ny == ty && nx < tx)) { tx = nx; ty = ny } }
                            }
                        }
                    }
                }
                if (tx != -1) {
                    td = min_d
                    do_bfs(tx, ty)
                    for (j=1; j<=4; j++) {
                        nx = u_x[id]+dx[j]
                        ny = u_y[id]+dy[j]
                        if (v_tag[ny, nx] == tag && dist[ny, nx] == td - 1) {
                            unit_at[u_y[id], u_x[id]] = 0
                            u_x[id] = nx
                            u_y[id] = ny
                            unit_at[ny, nx] = id
                            break
                        }
                    }
                }
            }
            m_hp = 999
            t_id = 0
            for (j=1; j<=4; j++) {
                nx = u_x[id]+dx[j]
                ny = u_y[id]+dy[j]
                tid = unit_at[ny, nx]
                if (tid > 0 && u_type[tid] != u_type[id]) {
                    if (u_hp[tid] < m_hp) { m_hp = u_hp[tid]; t_id = tid }
                }
            }
            if (t_id > 0) {
                u_hp[t_id] -= 3
                if (u_hp[t_id] <= 0) {
                    u_alive[t_id] = 0
                    unit_at[u_y[t_id], u_x[t_id]] = 0
                }
            }
        }
        if (!round_done) break
        rounds++
    }
    hp_sum = 0
    for (k=1; k<=u_cnt; k++) if (u_alive[k]) hp_sum += u_hp[k]
    print rounds * hp_sum
}
' input.txt
