
#!/usr/bin/awk -f

# Normalizes points and generates a unique key for the orientation
function get_key_and_normalize(n,   i, j, min_r, min_c, key, t) {
    min_r = 1000; min_c = 1000
    for (i = 0; i < n; i++) {
        if (tmp_pts_r[i] < min_r) min_r = tmp_pts_r[i]
        if (tmp_pts_c[i] < min_c) min_c = tmp_pts_c[i]
    }
    for (i = 0; i < n; i++) {
        tmp_pts_r[i] -= min_r
        tmp_pts_c[i] -= min_c
    }
    # Sort points for a consistent key
    for (i = 0; i < n - 1; i++) {
        for (j = i + 1; j < n; j++) {
            if (tmp_pts_r[i] > tmp_pts_r[j] || (tmp_pts_r[i] == tmp_pts_r[j] && tmp_pts_c[i] > tmp_pts_c[j])) {
                t = tmp_pts_r[i]; tmp_pts_r[i] = tmp_pts_r[j]; tmp_pts_r[j] = t
                t = tmp_pts_c[i]; tmp_pts_c[i] = tmp_pts_c[j]; tmp_pts_c[j] = t
            }
        }
    }
    key = ""
    for (i = 0; i < n; i++) key = key tmp_pts_r[i] "," tmp_pts_c[i] " "
    return key
}

# Adds a unique orientation for a given shape
function add_orient(sid, n,   key, o, i) {
    key = get_key_and_normalize(n)
    if (unique_orient[sid, key]) return
    unique_orient[sid, key] = 1
    o = num_orients[sid]++
    max_r[sid, o] = 0; max_c[sid, o] = 0
    for (i = 0; i < n; i++) {
        shapes_r[sid, o, i] = tmp_pts_r[i]
        shapes_c[sid, o, i] = tmp_pts_c[i]
        if (tmp_pts_r[i] > max_r[sid, o]) max_r[sid, o] = tmp_pts_r[i]
        if (tmp_pts_c[i] > max_c[sid, o]) max_c[sid, o] = tmp_pts_c[i]
    }
}

# Generates all possible symmetries (rotations/flips)
function generate_symmetries(sid, n,   i, j, t) {
    for (i = 0; i < n; i++) {
        tmp_pts_r[i] = orig_r[sid, i]; tmp_pts_c[i] = orig_c[sid, i]
    }
    for (j = 0; j < 4; j++) {
        for (i = 0; i < n; i++) {
            t = tmp_pts_r[i]; tmp_pts_r[i] = tmp_pts_c[i]; tmp_pts_c[i] = -t
        }
        add_orient(sid, n)
    }
    for (i = 0; i < n; i++) {
        tmp_pts_r[i] = orig_r[sid, i]; tmp_pts_c[i] = -orig_c[sid, i]
    }
    for (j = 0; j < 4; j++) {
        for (i = 0; i < n; i++) {
            t = tmp_pts_r[i]; tmp_pts_r[i] = tmp_pts_c[i]; tmp_pts_c[i] = -t
        }
        add_orient(sid, n)
    }
}

# Backtracking search to fit pieces into the region
function solve(p_idx, s_pos, s_orient,   id, start_f_pos, o_start_base, pos, r, c, o_start, o, i, ok) {
    if (p_idx > total_req) return 1
    if (current_area + remaining_area_needed[p_idx] > max_area) return 0
    
    id = req_ids[p_idx]
    if (id == req_ids[p_idx-1]) {
        start_f_pos = s_pos
        o_start_base = s_orient
    } else {
        start_f_pos = 0
        o_start_base = 0
    }

    for (pos = start_f_pos; pos < total_cells; pos++) {
        r = int(pos / region_w); c = pos % region_w
        o_start = (pos == start_f_pos) ? o_start_base : 0
        
        for (o = o_start; o < num_orients[id]; o++) {
            if (r + max_r[id, o] < region_h && c + max_c[id, o] < region_w) {
                ok = 1
                for (i = 0; i < shape_pts_count[id]; i++) {
                    if (grid[pos + shapes_off[id, o, i]]) { ok = 0; break }
                }
                if (ok) {
                    for (i = 0; i < shape_pts_count[id]; i++) grid[pos + shapes_off[id, o, i]] = 1
                    current_area += shape_pts_count[id]
                    if (solve(p_idx + 1, pos, o)) return 1
                    for (i = 0; i < shape_pts_count[id]; i++) grid[pos + shapes_off[id, o, i]] = 0
                    current_area -= shape_pts_count[id]
                }
            }
        }
    }
    return 0
}

BEGIN {
    # Ensure input is read from input.txt
    if (ARGC < 2) { ARGV[1] = "input.txt"; ARGC = 2 }
    num_shapes = 0
    cur_sid = -1
    success_count = 0
}

# Parsing Logic
{
    if ($0 ~ /^[0-9]+:$/) {
        cur_sid = substr($1, 1, length($1)-1)
        if (cur_sid >= num_shapes) num_shapes = cur_sid + 1
        cur_row = 0
    } else if ($0 ~ /^[#.]+$/ && cur_sid != -1) {
        for (i = 1; i <= length($0); i++) {
            if (substr($0, i, 1) == "#") {
                p = shape_pts_count[cur_sid]++; orig_r[cur_sid, p] = cur_row; orig_c[cur_sid, p] = i-1
            }
        }
        cur_row++
    } else if ($0 ~ /^[0-9]+x[0-9]+:/) {
        if (cur_sid != -1) {
            for (s = 0; s < num_shapes; s++) generate_symmetries(s, shape_pts_count[s])
            cur_sid = -1
        }
        
        split($0, parts, ":")
        split(parts[1], dims, "x")
        region_w = dims[1]; region_h = dims[2]
        total_cells = region_w * region_h
        max_area = total_cells
        
        sub(/^[ ]+/, "", parts[2])
        n_counts = split(parts[2], counts, /[ ]+/)
        total_req = 0; total_area_needed = 0
        for (i = 1; i <= n_counts; i++) {
            sid = i - 1
            for (j = 0; j < counts[i]; j++) {
                req_ids[++total_req] = sid; total_area_needed += shape_pts_count[sid]
            }
        }
        
        if (total_area_needed <= max_area) {
            # Heuristic: process largest pieces first
            for (i = 1; i < total_req; i++) {
                for (j = i + 1; j <= total_req; j++) {
                    if (shape_pts_count[req_ids[i]] < shape_pts_count[req_ids[j]]) {
                        t = req_ids[i]; req_ids[i] = req_ids[j]; req_ids[j] = t
                    }
                }
            }
            remaining_area_needed[total_req + 1] = 0
            for (i = total_req; i >= 1; i--) {
                remaining_area_needed[i] = remaining_area_needed[i+1] + shape_pts_count[req_ids[i]]
            }
            for (i = 1; i <= total_req; i++) {
                id = req_ids[i]
                for (o = 0; o < num_orients[id]; o++) {
                    for (p = 0; p < shape_pts_count[id]; p++)
                        shapes_off[id, o, p] = shapes_r[id, o, p] * region_w + shapes_c[id, o, p]
                }
            }
            for (i = 0; i < total_cells; i++) grid[i] = 0
            current_area = 0; req_ids[0] = -1 
            if (solve(1, 0, 0)) success_count++
        }
    }
}

END {
    print success_count
}

