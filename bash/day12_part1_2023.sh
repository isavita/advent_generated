
#!/bin/bash

# AWK script for processing and calculation
read -r -d '' AWK_SCRIPT <<'EOF'
function count(springs, n_s, groups, n_g, i_s, i_g, i_contig,    # Local vars start here
               cache_key, res, char, cached_val) {

    # Base Case: Reached end of springs string
    if (i_s > n_s) {
        if (i_g > n_g && i_contig == 0) {
            return 1 # All groups matched, no partial group at the end
        }
        if (i_g == n_g && i_contig == groups[i_g]) {
             return 1 # Last group matched exactly at the end
        }
        return 0 # Mismatch
    }

    # Memoization Check
    cache_key = i_s SUBSEP i_g SUBSEP i_contig
    if (cache_key in cache) {
        return cache[cache_key]
    }

    res = 0
    char = substr(springs, i_s, 1)

    # Case 1: Try placing '.' (Operational or Unknown as Operational)
    if (char == "." || char == "?") {
        if (i_contig == 0) {
            # Not in a damaged group, just move to next spring
            res += count(springs, n_s, groups, n_g, i_s + 1, i_g, 0)
        } else if (i_contig == groups[i_g]) {
            # Just finished a damaged group, move to next spring and next group
             res += count(springs, n_s, groups, n_g, i_s + 1, i_g + 1, 0)
        }
        # If i_contig > 0 but != groups[i_g], this path is invalid for '.'
    }

    # Case 2: Try placing '#' (Damaged or Unknown as Damaged)
    if (char == "#" || char == "?") {
        if (i_g <= n_g && i_contig < groups[i_g]) {
            # Continue current damaged group
            res += count(springs, n_s, groups, n_g, i_s + 1, i_g, i_contig + 1)
        }
        # If i_g > n_g or i_contig >= groups[i_g], this path is invalid for '#'
    }

    cache[cache_key] = res
    return res
}

# Main processing block for each line
{
    n_s = length($1)
    n_g = split($2, groups_arr, ",")

    # Convert group strings to numbers (awk does this often automatically, but explicit is safer)
    for (i = 1; i <= n_g; i++) {
        groups_num[i] = groups_arr[i] + 0
    }

    # Clear cache for each new line/row
    delete cache

    # Calculate arrangements for the current row and add to total
    # AWK arrays/strings are 1-indexed
    total_arrangements += count($1, n_s, groups_num, n_g, 1, 1, 0)
}

# After processing all lines
END {
    print total_arrangements
}
EOF

# Main function for bash script execution
main() {
    local input_file="input.txt"

    if [[ ! -f "$input_file" ]]; then
        echo "Error: Input file '$input_file' not found." >&2
        exit 1
    fi

    # Execute the AWK script, passing the input file
    awk "$AWK_SCRIPT" "$input_file"
}

# Script entry point
main
