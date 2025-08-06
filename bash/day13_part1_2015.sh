
#!/usr/bin/env bash
#  Solve the seating‑happiness problem (Advent of Code 2015 – Day 13)
#  Reads the input from input.txt and prints the maximum total happiness.

awk '
BEGIN {
    # Map guest names to numeric indices
    n = 0
}
{
    # Parse a line such as:
    #   Alice would gain 54 happiness units by sitting next to Bob.
    from = $1
    action = $3
    value = $4
    to = $NF
    sub(/\./, "", to)          # remove trailing period

    if (action == "lose") value = -value

    # Assign indices to guests
    if (!(from in idx)) { idx[from] = n; names[n] = from; n++ }
    if (!(to in idx))   { idx[to]   = n; names[n] = to;   n++ }

    # Store happiness value
    a[idx[from] SUBSEP idx[to]] = value
}
END {
    # Prepare array of indices for permutation
    for (i = 0; i < n; i++) perm[i] = i

    max = 0
    permute(0)

    print max
}

# Recursive permutation generator
function permute(pos,    i, j, tmp, sum, left, right, k) {
    if (pos == n) {
        sum = 0
        for (i = 0; i < n; i++) {
            left  = (i + 1) % n
            right = (i - 1 + n) % n
            sum += a[perm[i] SUBSEP perm[left]] + a[perm[i] SUBSEP perm[right]]
        }
        if (sum > max) max = sum
        return
    }
    for (i = pos; i < n; i++) {
        tmp = perm[pos]; perm[pos] = perm[i]; perm[i] = tmp
        permute(pos + 1)
        tmp = perm[pos]; perm[pos] = perm[i]; perm[i] = tmp
    }
}
' input.txt
