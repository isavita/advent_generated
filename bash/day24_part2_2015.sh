
#!/bin/bash

# Use awk to handle the recursive combination search efficiently.
# It finds the smallest group size (k) that sums to target (total/4).
# Among those, it finds the one with the minimum Quantum Entanglement (product of weights).
awk '
{
    p[n++] = $1
    s += $1
}
END {
    target = s / 4
    # Sort packages descending to optimize the search
    for (i = 0; i < n; i++) {
        for (j = i + 1; j < n; j++) {
            if (p[i] < p[j]) {
                tmp = p[i]; p[i] = p[j]; p[j] = tmp
            }
        }
    }

    # Iterate through possible group sizes k
    for (k = 1; k <= n; k++) {
        min_qe = 0
        find_group(0, target, k, 1)
        if (min_qe > 0) {
            printf "%.0f\n", min_qe
            exit
        }
    }
}

function find_group(idx, rem, len, qe,    i) {
    # Base case: found a combination of length k
    if (len == 0) {
        if (rem == 0) {
            if (min_qe == 0 || qe < min_qe) min_qe = qe
        }
        return
    }

    # Optimization: pruning
    for (i = idx; i <= n - len; i++) {
        # If current largest available element * remaining length < remaining sum, 
        # we cannot reach the target because elements are sorted descending.
        if (p[i] * len < rem) break
        
        if (p[i] <= rem) {
            # Further pruning: if current QE already exceeds known min_qe
            if (min_qe != 0 && qe * p[i] >= min_qe) continue
            
            find_group(i + 1, rem - p[i], len - 1, qe * p[i])
        }
    }
}
' input.txt
