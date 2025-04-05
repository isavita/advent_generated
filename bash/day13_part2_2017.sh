
#!/bin/bash

# Reads firewall configuration from input.txt and finds the smallest delay
# to pass through without being caught.

main() {
    # Use awk for efficient processing:
    # -F': *' sets the field separator to ':' followed by optional spaces.
    # Reads depth:range into the associative array fw[depth] = range.
    # In the END block, it iterates through delays starting from 0.
    # For each delay, it checks all firewall layers.
    # If caught at any layer ((delay + depth) % (2 * (range - 1)) == 0),
    # it increments the delay and tries again.
    # Assumes range > 1, as range=1 would lead to modulo by zero.
    # Prints the first delay for which no layer catches the packet.
    awk '
    BEGIN { FS = ": *" }
    { fw[$1] = $2 }
    END {
        delay = 0
        while (1) {
            caught = 0
            for (depth in fw) {
                range = fw[depth]
                period = 2 * (range - 1)
                # Check if period is zero (range=1). If so, cannot be caught unless depth=0 and delay=0
                # However, Python original code would raise ZeroDivisionError if range=1,
                # implying inputs have range > 1. We follow that assumption.
                if (period > 0 && (delay + depth) % period == 0) {
                    caught = 1
                    break # Caught on this layer, no need to check others for this delay
                }
            }
            if (caught == 0) {
                print delay
                exit # Found the first safe delay
            }
            delay++
        }
    }
    ' "input.txt"
}

# Execute the main function
main
