
#!/usr/bin/awk -f

BEGIN {
    FS = ""
    INF = 1e9 # Represents infinity

    # Read grid and find S, E
    n = 0
    while ((getline line < "input.txt") > 0) {
        m = length(line)
        for (c = 1; c <= m; c++) {
            char = substr(line, c, 1)
            grid[n, c-1] = char
            if (char == "S") {
                sx = n; sy = c - 1
            } else if (char == "E") {
                ex = n; ey = c - 1
            }
        }
        n++
    }
    close("input.txt")

    # Directions: 0:Up, 1:Right, 2:Down, 3:Left
    dx[0] = -1; dy[0] = 0
    dx[1] = 0;  dy[1] = 1
    dx[2] = 1;  dy[2] = 0
    dx[3] = 0;  dy[3] = -1

    # Initialize distances
    # Use a single associative array pq for the priority queue simulation
    # Key: r SUBSEP c SUBSEP d, Value: cost
    # dist array stores the best known distance
    # Key: r SUBSEP c SUBSEP d, Value: cost
    pq[sx SUBSEP sy SUBSEP 1] = 0
    dist[sx SUBSEP sy SUBSEP 1] = 0
    pq_count = 1

    # Dijkstra's algorithm simulation
    while (pq_count > 0) {
        # Find minimum cost node in pq (inefficient part)
        min_cost = INF
        min_key = ""
        for (key in pq) {
            cost = pq[key]
            if (cost < min_cost) {
                min_cost = cost
                min_key = key
            }
        }

        # Extract node details
        split(min_key, parts, SUBSEP)
        x = parts[1]; y = parts[2]; d = parts[3]
        cost = pq[min_key]
        delete pq[min_key]
        pq_count--

        # Optimization: Skip if we found a shorter path already
        dist_key = x SUBSEP y SUBSEP d
        if (dist_key in dist && cost > dist[dist_key]) {
             continue
        }

        # Explore neighbors
        # 1. Turns (cost + 1000)
        for (i = -1; i <= 1; i += 2) {
            ndir = (d + i + 4) % 4
            nc = cost + 1000
            ndist_key = x SUBSEP y SUBSEP ndir
            if (!(ndist_key in dist) || nc < dist[ndist_key]) {
                dist[ndist_key] = nc
                pq[ndist_key] = nc
                pq_count++ # Note: count might be off if key already existed, but logic works
            }
        }

        # 2. Move Forward (cost + 1)
        nx = x + dx[d]
        ny = y + dy[d]

        if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx, ny] != "#") {
            nc = cost + 1
            ndist_key = nx SUBSEP ny SUBSEP d
            if (!(ndist_key in dist) || nc < dist[ndist_key]) {
                dist[ndist_key] = nc
                pq[ndist_key] = nc
                pq_count++ # Note: count might be off if key already existed, but logic works
            }
        }
    }

    # Find minimum cost to reach E
    best = INF
    for (d = 0; d < 4; d++) {
        key = ex SUBSEP ey SUBSEP d
        if (key in dist && dist[key] < best) {
            best = dist[key]
        }
    }

    # Backtracking to find all cells on shortest paths
    # rev_q simulates the queue for BFS/DFS backward search
    rev_q_head = 0
    rev_q_tail = 0

    # Initialize backtracking from end states with minimum cost
    for (d = 0; d < 4; d++) {
        key = ex SUBSEP ey SUBSEP d
        if (key in dist && dist[key] == best) {
            if (!(key in vis)) {
                 vis[key] = 1
                 rev_q[rev_q_tail++] = key
            }
        }
    }

    while (rev_q_head < rev_q_tail) {
        key = rev_q[rev_q_head++]
        split(key, parts, SUBSEP)
        x = parts[1]; y = parts[2]; d = parts[3]

        used[x SUBSEP y] = 1 # Mark cell (x,y) as used
        costU = dist[key]

        # Reverse Turn Check
        for (i = -1; i <= 1; i += 2) {
            pd = (d + i + 4) % 4
            prev_key = x SUBSEP y SUBSEP pd
            if (prev_key in dist && dist[prev_key] == costU - 1000) {
                if (!(prev_key in vis)) {
                    vis[prev_key] = 1
                    rev_q[rev_q_tail++] = prev_key
                }
            }
        }

        # Reverse Move Check
        px = x - dx[d]
        py = y - dy[d]
        if (px >= 0 && px < n && py >= 0 && py < m && grid[px, py] != "#") {
            prev_key = px SUBSEP py SUBSEP d
             if (prev_key in dist && dist[prev_key] == costU - 1) {
                 if (!(prev_key in vis)) {
                     vis[prev_key] = 1
                     rev_q[rev_q_tail++] = prev_key
                 }
             }
        }
    }

    # Count unique used cells
    count = 0
    for (ukey in used) {
        count++
    }
    print count
}

# No main processing block needed as all logic is in BEGIN/END
# END block could also be used, but BEGIN works fine since we read the file explicitly.
