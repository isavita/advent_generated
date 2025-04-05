
#!/bin/bash

main() {
    awk '
    # Parse initial state and steps
    NR==1 { initial_state = substr($NF, 1, length($NF)-1); next }
    NR==2 { steps = $(NF-1); next }

    # Skip blank lines
    /^\s*$/ { next }

    # Parse state definitions
    /In state/ { current_state_key = substr($NF, 1, length($NF)-1); next }
    /If the current value is/ { current_val_key = substr($NF, 1, length($NF)-1); next }
    /- Write the value/ { write_val[current_state_key, current_val_key] = substr($NF, 1, length($NF)-1); next }
    /- Move one slot to the/ { move_dir[current_state_key, current_val_key] = ($NF == "right." ? 1 : -1); next }
    /- Continue with state/ { next_state[current_state_key, current_val_key] = substr($NF, 1, length($NF)-1); next }

    END {
        # Initialize simulation
        state = initial_state
        cursor = 0
        # tape array defaults values to 0 or "" which acts as 0 in numeric context

        # Run simulation
        for (i = 0; i < steps; i++) {
            current_val = tape[cursor] + 0 # Get value at cursor, default 0
            idx = state SUBSEP current_val # Create composite key for lookup

            w = write_val[idx]
            m = move_dir[idx]
            n = next_state[idx]

            if (w == 1) {
                tape[cursor] = 1
            } else {
                # Optimization: only store 1s, delete if writing 0
                # Helps keep the tape array smaller if values frequently switch back to 0
                delete tape[cursor]
            }
            cursor += m
            state = n
        }

        # Calculate checksum (sum of values in tape, which are all 1s)
        checksum = 0
        for (pos in tape) {
            checksum++ # Since only 1s are stored, just count the elements
        }
        print checksum
    }
    ' input.txt
}

main
