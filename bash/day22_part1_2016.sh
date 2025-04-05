
#!/bin/bash

# main entry point
main() {
    # Use awk to process the input file efficiently
    # NR>2 skips the first two header lines, similar to the Python regex approach
    # $1 ~ /node-x[0-9]+-y[0-9]+/ ensures we only process node lines
    # Store Used (field 3 minus 'T') and Avail (field 4 minus 'T') in arrays
    # In the END block, iterate through all pairs and count viable ones
    awk '
    NR > 2 && /node-x[0-9]+-y[0-9]+/ {
        # Extract Used (field 3) and Avail (field 4), removing trailing 'T'
        u = $3; sub(/T$/, "", u)
        a = $4; sub(/T$/, "", a)
        # Store in arrays, using a dedicated index n
        idx = ++n
        used[idx] = u + 0   # Force numeric
        avail[idx] = a + 0  # Force numeric
    }
    END {
        count = 0
        # Iterate through all pairs of nodes (i, j)
        for (i = 1; i <= n; i++) {
            for (j = 1; j <= n; j++) {
                # Check conditions: i != j, used[i] > 0, used[i] <= avail[j]
                if (i != j && used[i] > 0 && used[i] <= avail[j]) {
                    count++
                }
            }
        }
        print count
    }' input.txt
}

# Execute the main function
main
