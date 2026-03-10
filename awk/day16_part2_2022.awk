
#!/usr/bin/awk -f

# Function to calculate maximum pressure released using Depth-First Search
function dfs(u, time, mask, pressure, results,   i, v, d, rem, m) {
    # If the current flow is better than the stored flow for this set of valves, update it.
    if (pressure > results[mask]) {
        results[mask] = pressure
    }

    # Iterate through all valves with positive flow rates.
    for (i = 0; i < useful_count; i++) {
        v = useful_list[i]
        m = bit_val[v]
        # Check if valve 'v' is already opened by checking its bit in the mask.
        if (int(mask / m) % 2 == 0) {
            d = dist[u SUBSEP v]
            # Time left after moving to valve 'v' and opening it (1 min to open).
            rem = time - d - 1
            if (rem > 0) {
                # Recurse with updated time, mask, and total pressure released.
                dfs(v, rem, mask + m, pressure + (rem * rates[v]), results)
            }
        }
    }
}

BEGIN {
    # Read the input file input.txt
    ARGV[1] = "input.txt"
    ARGC = 2
    inf = 999
}

{
    # Parse valve name and its flow rate.
    v = $2
    if (match($0, /rate=[0-9]+/)) {
        s = substr($0, RSTART + 5, RLENGTH - 5)
        rates[v] = s + 0
    }
    
    # Track all valve names.
    valves[v] = 1
    
    # Parse tunnel connections. Neighbors start after the word "valve" or "valves".
    for (i = 1; i <= NF; i++) {
        if ($i ~ /^valves?$/) {
            for (j = i + 1; j <= NF; j++) {
                n = $j
                gsub(/,/, "", n)
                dist[v SUBSEP n] = 1
            }
            break
        }
    }
}

END {
    # Initialize the distance matrix for Floyd-Warshall.
    for (i in valves) {
        for (j in valves) {
            if (i == j) {
                dist[i SUBSEP j] = 0
            } else if (!(i SUBSEP j in dist)) {
                dist[i SUBSEP j] = inf
            }
        }
    }

    # Floyd-Warshall algorithm to find all-pairs shortest paths.
    for (k in valves) {
        for (i in valves) {
            ik = i SUBSEP k
            if (dist[ik] == inf) continue
            for (j in valves) {
                kj = k SUBSEP j
                ij = i SUBSEP j
                if (dist[ik] + dist[kj] < dist[ij]) {
                    dist[ij] = dist[ik] + dist[kj]
                }
            }
        }
    }

    # Identify useful valves (those with flow rate > 0) and assign bitmasks.
    useful_count = 0
    p2 = 1
    for (v in rates) {
        if (rates[v] > 0) {
            useful_list[useful_count] = v
            bit_val[v] = p2
            p2 *= 2
            useful_count++
        }
    }
    total_masks = p2

    # --- Part 1 ---
    # Max pressure one person can release in 30 minutes starting at "AA".
    dfs("AA", 30, 0, 0, res1)
    max1 = 0
    for (m in res1) {
        if (res1[m] > max1) max1 = res1[m]
    }
    print "Part 1: " max1

    # --- Part 2 ---
    # Max pressure two people (you and an elephant) can release in 26 minutes.
    dfs("AA", 26, 0, 0, res2)
    
    # Sum Over Subsets (SOS) optimization:
    # Update res2[mask] to store the max pressure for any subset of the given mask.
    for (i = 0; i < useful_count; i++) {
        m = 2^i
        for (mask = 0; mask < total_masks; mask++) {
            if (int(mask / m) % 2 == 1) {
                prev = mask - m
                if (res2[prev] > res2[mask]) {
                    res2[mask] = res2[prev]
                }
            }
        }
    }

    # Find the maximum combined flow for two disjoint sets of valves.
    max2 = 0
    for (mask = 0; mask < total_masks; mask++) {
        complement = (total_masks - 1) - mask
        # You take 'mask', elephant takes 'complement'.
        if (res2[mask] + res2[complement] > max2) {
            max2 = res2[mask] + res2[complement]
        }
    }
    print "Part 2: " max2
}

