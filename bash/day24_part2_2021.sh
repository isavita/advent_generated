
#!/bin/bash

# Main function encapsulates the script logic
main() {
    # Use awk to process the input file efficiently
    awk '
    # NR is 1-based line number. Python index i = NR - 1.
    # Adjust modulo checks accordingly:
    # i % 18 == 4  => NR % 18 == 5
    # i % 18 == 5  => NR % 18 == 6
    # i % 18 == 15 => NR % 18 == 16
    BEGIN {
        idx = 0 # Index for arrays k, l, m (0-13)
        sp = 0  # Stack pointer
    }

    # Extract relevant numbers into arrays based on line number modulo 18
    NR % 18 == 5 { l[idx] = $3 }
    NR % 18 == 6 { k[idx] = $3 }
    NR % 18 == 16 { m[idx] = $3; idx++ } # Increment index only after the last relevant line of a block

    END {
        # Calculate constraints using stack simulation
        for (i = 0; i < 14; i++) { # Expecting 14 blocks -> 14 entries
            if (l[i] == 1) {
                stack[sp++] = i # Push index onto stack
            } else if (l[i] == 26) {
                pop = stack[--sp] # Pop index from stack
                # constraints[pop] = [i, m[pop] + k[i]]
                constraints_val[pop] = m[pop] + k[i] # Store the value difference (m[pop] + k[i])
                constraints_idx[pop] = i           # Store the related index (i)
            }
        }

        # Determine minimum digits (1-9) satisfying constraints
        # min_val[pop] = vmin
        # min_val[i] = vmin + diff
        # We need 1 <= vmin <= 9  AND  1 <= vmin + diff <= 9
        # From the second: 1 - diff <= vmin <= 9 - diff
        # Combined: max(1, 1 - diff) <= vmin <= min(9, 9 - diff)
        # To minimize the overall number, we choose the smallest possible vmin.
        # So, vmin = max(1, 1 - diff)
        for (pop_idx in constraints_val) { # Iterate over the indices that pushed onto stack
            diff = constraints_val[pop_idx]
            pair_idx = constraints_idx[pop_idx]

            vmin = 1
            if (1 - diff > vmin) {
                vmin = 1 - diff
            }

            min_val[pop_idx] = vmin
            min_val[pair_idx] = vmin + diff
        }

        # Construct and print the final 14-digit number
        result = ""
        for (i = 0; i < 14; i++) {
            result = result min_val[i]
        }
        print result
    }
    ' input.txt # Read input from input.txt
}

# Execute the main function
main
