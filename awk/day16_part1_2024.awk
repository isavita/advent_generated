
#!/usr/bin/awk -f

BEGIN {
    rows = 0
    while (getline line < "input.txt" > 0) {
        cols = length(line)
        for (c = 1; c <= cols; c++) {
            char = substr(line, c, 1)
            grid[rows "," c-1] = char
            if (char == "S") { start_r = rows; start_c = c-1 }
            if (char == "E") { end_r = rows; end_c = c-1 }
        }
        rows++
    }
    close("input.txt")

    dr[0]=0;  dc[0]=1
    dr[1]=1;  dc[1]=0
    dr[2]=0;  dc[2]=-1
    dr[3]=-1; dc[3]=0

    start_key = start_r "," start_c "," 0
    pq[start_key] = 0

    while (length(pq) > 0) {
        min_cost = -1
        min_key = ""
        for (key in pq) {
            if (min_key == "" || pq[key] < min_cost) {
                min_cost = pq[key]
                min_key = key
            }
        }

        split(min_key, state, ",")
        r = state[1]
        c = state[2]
        dir_idx = state[3]

        delete pq[min_key]

        if (min_key in visited) {
            continue
        }
        visited[min_key] = 1

        if (r == end_r && c == end_c) {
            print min_cost
            exit
        }

        # Move Forward
        nr = r + dr[dir_idx]
        nc = c + dc[dir_idx]
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr "," nc] != "#") {
             next_key = nr "," nc "," dir_idx
             next_cost = min_cost + 1
             if (!(next_key in visited)) {
                 if (!(next_key in pq) || next_cost < pq[next_key]) {
                     pq[next_key] = next_cost
                 }
             }
        }

        # Rotate Clockwise
        next_dir_cw = (dir_idx + 1) % 4
        next_key_cw = r "," c "," next_dir_cw
        next_cost_cw = min_cost + 1000
        if (!(next_key_cw in visited)) {
             if (!(next_key_cw in pq) || next_cost_cw < pq[next_key_cw]) {
                 pq[next_key_cw] = next_cost_cw
             }
        }

        # Rotate Counter-Clockwise
        next_dir_ccw = (dir_idx - 1 + 4) % 4
        next_key_ccw = r "," c "," next_dir_ccw
        next_cost_ccw = min_cost + 1000
         if (!(next_key_ccw in visited)) {
             if (!(next_key_ccw in pq) || next_cost_ccw < pq[next_key_ccw]) {
                 pq[next_key_ccw] = next_cost_ccw
             }
        }
    }
}

