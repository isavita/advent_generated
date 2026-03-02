
function abs(x) { return x < 0 ? -x : x }

function move_char(s, i, j,   v1, v2, t, tmp) {
    v1 = substr(s, i, 1); v2 = substr(s, j, 1)
    if (i > j) { t = i; i = j; j = t; tmp = v1; v1 = v2; v2 = tmp }
    return substr(s, 1, i-1) v2 substr(s, i+1, j-i-1) v1 substr(s, j+1)
}

function push(c, s,   i, p, tmp_v, tmp_s) {
    i = ++q_len
    q_val[i] = c; q_state[i] = s
    while (i > 1) {
        p = int(i/2)
        if (q_val[p] <= q_val[i]) break
        tmp_v = q_val[i]; q_val[i] = q_val[p]; q_val[p] = tmp_v
        tmp_s = q_state[i]; q_state[i] = q_state[p]; q_state[p] = tmp_s
        i = p
    }
}

function pop(   res_c, res_s, i, child, tmp_v, tmp_s) {
    if (q_len <= 0) return ""
    res_c = q_val[1]; res_s = q_state[1]
    q_val[1] = q_val[q_len]; q_state[1] = q_state[q_len--]
    i = 1
    while ((child = i*2) <= q_len) {
        if (child < q_len && q_val[child+1] < q_val[child]) child++
        if (q_val[i] <= q_val[child]) break
        tmp_v = q_val[i]; q_val[i] = q_val[child]; q_val[child] = tmp_v
        tmp_s = q_state[i]; q_state[i] = q_state[child]; q_state[child] = tmp_s
        i = child
    }
    return res_c " " res_s
}

function process(new_s, extra_c, base_c,   new_c) {
    new_c = base_c + extra_c
    if (!(new_s in seen) || new_c < seen[new_s]) {
        seen[new_s] = new_c
        push(new_c, new_s)
    }
}

function get_moves(state, base_c,   i, char, target_room_idx, target_h_idx, can_enter, deepest_pos, depth, r_char, path_clear, step, h_idx, room_idx, top_depth, room_char, start_h_idx, p, should_move) {
    for (i = 1; i <= 11; i++) {
        char = substr(state, i, 1)
        if (char == ".") continue
        target_room_idx = (index("ABCD", char) - 1) * 4 + 12
        target_h_idx = (index("ABCD", char) * 2) + 1
        can_enter = 1; deepest_pos = 0
        for (depth = 0; depth < 4; depth++) {
            r_char = substr(state, target_room_idx + depth, 1)
            if (r_char == ".") deepest_pos = depth + 1
            else if (r_char != char) { can_enter = 0; break }
        }
        if (can_enter && deepest_pos > 0) {
            path_clear = 1
            step = (target_h_idx > i) ? 1 : -1
            for (h_idx = i + step; ; h_idx += step) {
                if (substr(state, h_idx, 1) != ".") { path_clear = 0; break }
                if (h_idx == target_h_idx) break
            }
            if (path_clear) process(move_char(state, i, target_room_idx + deepest_pos - 1), (abs(target_h_idx - i) + deepest_pos) * cost[char], base_c)
        }
    }
    for (room_idx = 12; room_idx <= 24; room_idx += 4) {
        top_depth = -1
        for (depth = 0; depth < 4; depth++) {
            if (substr(state, room_idx + depth, 1) != ".") { top_depth = depth; break }
        }
        if (top_depth == -1) continue
        room_char = substr("ABCD", (room_idx-12)/4 + 1, 1)
        should_move = 0
        for (depth = top_depth; depth < 4; depth++) {
            if (substr(state, room_idx + depth, 1) != room_char) { should_move = 1; break }
        }
        if (!should_move) continue
        char = substr(state, room_idx + top_depth, 1)
        start_h_idx = ((room_idx-12)/4 + 1) * 2 + 1
        for (h_idx = 1; h_idx <= 11; h_idx++) {
            if (h_idx == 3 || h_idx == 5 || h_idx == 7 || h_idx == 9) continue
            path_clear = 1
            step = (h_idx > start_h_idx) ? 1 : -1
            for (p = start_h_idx; ; p += step) {
                if (substr(state, p, 1) != ".") { path_clear = 0; break }
                if (p == h_idx) break
            }
            if (path_clear) process(move_char(state, room_idx + top_depth, h_idx), (abs(h_idx - start_h_idx) + top_depth + 1) * cost[char], base_c)
        }
    }
}

function get_amphis(str, arr,   k, i, c) {
    k = 0
    for (i=1; i<=length(str); i++) {
        c = substr(str, i, 1)
        if (c ~ /[ABCD]/) arr[++k] = c
    }
}

BEGIN {
    cost["A"] = 1; cost["B"] = 10; cost["C"] = 100; cost["D"] = 1000
    target = "...........AAAABBBBCCCCDDDD"
    while ((getline < "input.txt") > 0) lines[++line_count] = $0
    get_amphis(lines[3], r1); get_amphis(lines[4], r4)
    r2[1]="D"; r2[2]="C"; r2[3]="B"; r2[4]="A"; r3[1]="D"; r3[2]="B"; r3[3]="A"; r3[4]="C"
    state = "..........."
    for (i=1; i<=4; i++) state = state r1[i] r2[i] r3[i] r4[i]
    seen[state] = 0; push(0, state)
    while (q_len > 0) {
        split(pop(), p_arr)
        curr_c = p_arr[1] + 0; curr_s = p_arr[2]
        if (curr_s == target) { print curr_c; exit }
        if (seen[curr_s] < curr_c) continue
        get_moves(curr_s, curr_c)
    }
}
