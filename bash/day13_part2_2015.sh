
#!/bin/bash

# The task is solved using an awk script for efficient parsing and permutations.
# It reads from "input.txt", adds a neutral guest "You", and finds the max happiness.

awk '
{
    # Parse input: "Alice would gain 54 happiness units by sitting next to Bob."
    # $1 = Alice, $3 = gain/lose, $4 = 54, $11 = Bob.
    u = $1
    v = $11
    sub(/\./, "", v)
    val = ($3 == "gain" ? $4 : -$4)
    
    # Store happiness units in an adjacency map
    h[u, v] = val
    guests[u] = 1
    guests[v] = 1
}
END {
    # Part 2: Add "You" as a neutral guest (happiness 0 with everyone)
    guests["You"] = 1
    
    # Convert guest names into a indexed array for permutations
    n = 0
    for (name in guests) {
        p[n++] = name
    }
    
    # Initialize max happiness and start permutations
    # Fix the first guest to optimize (circular permutations: (n-1)!)
    max_h = 0
    permute(1)
    print max_h
}

# Recursive Heap-like permutation function
function permute(k,    i, t, total, a, b) {
    if (k == n) {
        total = 0
        for (i = 0; i < n; i++) {
            a = p[i]
            b = p[(i + 1) % n]
            total += h[a, b] + h[b, a]
        }
        if (total > max_h) max_h = total
        return
    }
    
    for (i = k; i < n; i++) {
        # Swap
        t = p[k]; p[k] = p[i]; p[i] = t
        permute(k + 1)
        # Swap back
        t = p[k]; p[k] = p[i]; p[i] = t
    }
}' input.txt

