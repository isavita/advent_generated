BEGIN {
    dy[1] = -1; dx[1] = 0; dy[2] = 0; dx[2] = -1; dy[3] = 0; dx[3] = 1; dy[4] = 1; dx[4] = 0
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    height = NR
    width = length($0)
    for (i = 1; i <= width; i++) orig_grid[NR, i] = substr($0, i, 1)
}
function bfs(sy, sx, dists, qy, qx,    q_h, q_t, cy, cx, d, b, ny, nx) {
    delete dists
    delete qy
    delete qx
    q_h = 1; q_t = 1
    qy[1] = sy; qx[1] = sx
    dists[sy, sx] = 0
    while (q_h <= q_t) {
        cy = qy[q_h]; cx = qx[q_h++]
        d = dists[cy, cx]
        for (b = 1; b <= 4; b++) {
            ny = cy + dy[b]; nx = cx + dx[b]
            if (grid[ny, nx] == "." && !((ny, nx) in dists)) {
                dists[ny, nx] = d + 1
                qy[++q_t] = ny; qx[q_t] = nx
            }
        }
    }
}
function simulate(elf_p,    i, j, n, rounds, u, target_exists, in_range, v, b, ny, nx, best_dist, best_ty, best_tx, dist, move_y, move_x, min_d, min_hp, target_unit, total_hp, u1, u2, tmp, num_units, enemy_type, qy, qx, qy2, qx2, order, d_from_unit, d_from_target, y_c, x_c) {
    num_units = 0
    delete units_y; delete units_x; delete units_hp; delete units_type; delete units_pow; delete unit_at; delete grid
    for (y_c = 1; y_c <= height; y_c++) {
        for (x_c = 1; x_c <= width; x_c++) {
            grid[y_c, x_c] = orig_grid[y_c, x_c]
            if (grid[y_c, x_c] == "E" || grid[y_c, x_c] == "G") {
                num_units++
                units_y[num_units] = y_c; units_x[num_units] = x_c
                units_hp[num_units] = 200; units_type[num_units] = grid[y_c, x_c]
                units_pow[num_units] = (grid[y_c, x_c] == "E" ? elf_p : 3)
                unit_at[y_c, x_c] = num_units
            }
        }
    }
    rounds = 0
    while (1) {
        n = 0; delete order
        for (i = 1; i <= num_units; i++) if (units_hp[i] > 0) order[++n] = i
        for (i = 1; i < n; i++) {
            for (j = i + 1; j <= n; j++) {
                u1 = order[i]; u2 = order[j]
                if (units_y[u1] > units_y[u2] || (units_y[u1] == units_y[u2] && units_x[u1] > units_x[u2])) {
                    tmp = order[i]; order[i] = order[j]; order[j] = tmp
                }
            }
        }
        for (i = 1; i <= n; i++) {
            u = order[i]
            if (units_hp[u] <= 0) continue
            enemy_type = (units_type[u] == "E" ? "G" : "E")
            target_exists = 0
            for (v = 1; v <= num_units; v++) if (units_hp[v] > 0 && units_type[v] == enemy_type) { target_exists = 1; break }
            if (!target_exists) {
                total_hp = 0
                for (v = 1; v <= num_units; v++) if (units_hp[v] > 0) total_hp += units_hp[v]
                return rounds * total_hp
            }
            in_range = 0
            for (b = 1; b <= 4; b++) {
                ny = units_y[u] + dy[b]; nx = units_x[u] + dx[b]
                if (grid[ny, nx] == enemy_type) in_range = 1
            }
            if (!in_range) {
                bfs(units_y[u], units_x[u], d_from_unit, qy, qx)
                best_dist = 1e9; best_ty = 0
                for (v = 1; v <= num_units; v++) {
                    if (units_hp[v] > 0 && units_type[v] == enemy_type) {
                        for (b = 1; b <= 4; b++) {
                            ny = units_y[v] + dy[b]; nx = units_x[v] + dx[b]
                            if (grid[ny, nx] == "." && ((ny, nx) in d_from_unit)) {
                                dist = d_from_unit[ny, nx]
                                if (best_ty == 0 || dist < best_dist || (dist == best_dist && (ny < best_ty || (ny == best_ty && nx < best_tx)))) {
                                    best_dist = dist; best_ty = ny; best_tx = nx
                                }
                            }
                        }
                    }
                }
                if (best_ty > 0) {
                    bfs(best_ty, best_tx, d_from_target, qy2, qx2)
                    min_d = 1e9; move_y = 0
                    for (b = 1; b <= 4; b++) {
                        ny = units_y[u] + dy[b]; nx = units_x[u] + dx[b]
                        if (grid[ny, nx] == "." && ((ny, nx) in d_from_target)) {
                            if (d_from_target[ny, nx] < min_d) {
                                min_d = d_from_target[ny, nx]; move_y = ny; move_x = nx
                            }
                        }
                    }
                    if (move_y > 0) {
                        grid[units_y[u], units_x[u]] = "."
                        delete unit_at[units_y[u], units_x[u]]
                        units_y[u] = move_y; units_x[u] = move_x
                        grid[move_y, move_x] = units_type[u]
                        unit_at[move_y, move_x] = u
                    }
                }
            }
            target_unit = 0; min_hp = 1e9
            for (b = 1; b <= 4; b++) {
                ny = units_y[u] + dy[b]; nx = units_x[u] + dx[b]
                if (grid[ny, nx] == enemy_type) {
                    v = unit_at[ny, nx]
                    if (target_unit == 0 || units_hp[v] < min_hp) {
                        min_hp = units_hp[v]; target_unit = v
                    }
                }
            }
            if (target_unit > 0) {
                units_hp[target_unit] -= units_pow[u]
                if (units_hp[target_unit] <= 0) {
                    if (units_type[target_unit] == "E" && elf_p > 3) return -1
                    grid[units_y[target_unit], units_x[target_unit]] = "."
                    delete unit_at[units_y[target_unit], units_x[target_unit]]
                }
            }
        }
        rounds++
    }
}
END {
    p = 4
    while (1) {
        res = simulate(p)
        if (res != -1) {
            print res
            exit
        }
        p++
    }
}