
#!/bin/bash

# Solution using awk for processing the grid and implementing the algorithm

read_grid_and_solve() {
    awk '
    function init_neighbors() {
        # Directions: 0: Up, 1: Right, 2: Down, 3: Left
        DX[0] = -1; DY[0] = 0
        DX[1] = 0;  DY[1] = 1
        DX[2] = 1;  DY[2] = 0
        DX[3] = 0;  DY[3] = -1
        INF = 999999999
    }

    # Read grid and find S, E
    {
        grid[NR-1] = $0
        N = NR
        M = length($0)
        for (c = 1; c <= M; ++c) {
            char = substr($0, c, 1)
            if (char == "S") {
                sx = NR - 1; sy = c - 1
            } else if (char == "E") {
                ex = NR - 1; ey = c - 1
            }
        }
    }

    END {
        init_neighbors()

        # Initialize distances: dist[x, y, direction] = cost
        # Use SUBSEP for multidimensional array simulation
        start_key = sx SUBSEP sy SUBSEP 1 # Start facing right (d=1) as per python pq init
        dist[start_key] = 0

        # Priority Queue simulation (using an associative array to store keys)
        pq_keys[start_key] = 1
        pq_count = 1

        # Dijkstra main loop
        while (pq_count > 0) {
            # Find node with minimum distance in pq_keys
            min_cost = INF
            min_key = ""
            for (key in pq_keys) {
                if (dist[key] < min_cost) {
                    min_cost = dist[key]
                    min_key = key
                }
            }

            if (min_key == "") break # Should not happen if E is reachable

            # Extract info from the key with minimum distance
            split(min_key, coords, SUBSEP)
            x = coords[1]; y = coords[2]; d = coords[3]
            cost = dist[min_key]

            # Remove from simulated PQ and mark as visited/settled
            delete pq_keys[min_key]
            pq_count--
            visited[min_key] = 1 # Mark state as finally processed

            # Explore turning left/right
            for (turn = -1; turn <= 1; turn += 2) { # -1: left, +1: right
                ndir = (d + turn + 4) % 4 # +4 handles potential negative result before mod
                nc = cost + 1000
                nkey = x SUBSEP y SUBSEP ndir

                # If new cost is better and state not settled
                if (!(nkey in visited) && (!(nkey in dist) || nc < dist[nkey])) {
                    dist[nkey] = nc
                    # Add/update in PQ if not already settled
                    if (!(nkey in pq_keys)) {
                        pq_keys[nkey] = 1
                        pq_count++
                    }
                }
            }

            # Explore moving forward
            nx = x + DX[d]
            ny = y + DY[d]

            # Check bounds and if not a wall
            if (nx >= 0 && nx < N && ny >= 0 && ny < M && substr(grid[nx], ny + 1, 1) != "#") {
                nc = cost + 1
                nkey = nx SUBSEP ny SUBSEP d

                # If new cost is better and state not settled
                if (!(nkey in visited) && (!(nkey in dist) || nc < dist[nkey])) {
                    dist[nkey] = nc
                     # Add/update in PQ if not already settled
                    if (!(nkey in pq_keys)) {
                        pq_keys[nkey] = 1
                        pq_count++
                    }
                }
            }
        }

        # Find the minimum cost to reach E from any direction
        best_cost = INF
        for (d = 0; d < 4; ++d) {
            key = ex SUBSEP ey SUBSEP d
            if (key in dist && dist[key] < best_cost) {
                best_cost = dist[key]
            }
        }

        # If E is unreachable
        if (best_cost == INF) {
             print "E not reachable" # Or handle appropriately
             exit 1
        }

        # Backtracking to find all cells on any shortest path
        # Initialize backtracking queue (rev) with optimal end states
        rev_head = 0
        rev_tail = 0
        for (d = 0; d < 4; ++d) {
            key = ex SUBSEP ey SUBSEP d
            if (key in dist && dist[key] == best_cost) {
                 if (!(key in back_visited)) { # Avoid duplicates if multiple directions have same min cost
                     rev[rev_tail++] = key
                     back_visited[key] = 1
                 }
            }
        }

        while (rev_head < rev_tail) {
            curr_key = rev[rev_head++]
            split(curr_key, coords, SUBSEP)
            x = coords[1]; y = coords[2]; d = coords[3]

            used[x SUBSEP y] = 1 # Mark cell (x,y) as used
            costU = dist[curr_key]

            # Check previous state: Turning
            for (turn = -1; turn <= 1; turn += 2) {
                 pd = (d + turn + 4) % 4
                 prev_key = x SUBSEP y SUBSEP pd
                 # Check if the cost difference matches the turn cost
                 if (prev_key in dist && dist[prev_key] == costU - 1000) {
                     if (!(prev_key in back_visited)) {
                         back_visited[prev_key] = 1
                         rev[rev_tail++] = prev_key
                     }
                 }
            }

            # Check previous state: Moving forward (i.e., moving backward from current state)
            px = x - DX[d]
            py = y - DY[d]
            prev_key = px SUBSEP py SUBSEP d

            # Check bounds, not a wall, cost difference matches move cost
            if (px >= 0 && px < N && py >= 0 && py < M && substr(grid[px], py + 1, 1) != "#") {
                 if (prev_key in dist && dist[prev_key] == costU - 1) {
                     if (!(prev_key in back_visited)) {
                         back_visited[prev_key] = 1
                         rev[rev_tail++] = prev_key
                     }
                 }
            }
        }

        # Count the number of used cells
        count = 0
        for (cell_key in used) {
            count++
        }
        print count
    }
    ' input.txt
}

main() {
    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found."
        exit 1
    fi
    read_grid_and_solve
}

main "$@"
