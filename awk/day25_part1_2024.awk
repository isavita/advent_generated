
#!/usr/bin/awk -f

# --- Day 25: Code Chronicle ---
# Awk script to find the number of fitting lock/key pairs from schematics.

# BEGIN block: Initialize variables and set up processing environment
BEGIN {
    # Ensure input is read from "input.txt" if no other file is specified via arguments.
    # Awk automatically reads from files listed in ARGV after the script name.
    # If ARGC == 1 (only the script name itself is present), we add "input.txt".
    if (ARGC == 1) {
         ARGV[ARGC] = "input.txt"
         ARGC++
    }

    RS = ""       # Set Record Separator to blank lines to process blocks
    FS = "\n"     # Set Field Separator to newline to treat each line as a field

    lock_count = 0    # Counter for locks found
    key_count = 0     # Counter for keys found
    fit_count = 0     # Counter for fitting pairs

    num_cols = 5      # Schematics are 5 columns wide (based on problem description)
    total_rows = -1   # Store the number of rows in a schematic (determined from first block)
    max_h_allowed = -1 # Max combined height = total_rows - 2 (space between top/bottom markers)

    SUBSEP = ","      # Separator for simulating multi-dimensional arrays (e.g., locks[l,c])
}

# Main processing block: Executed for each record (schematic block)
{
    # Skip potential empty blocks caused by multiple consecutive blank lines
    if (NF == 0) next

    # Determine schematic dimensions and max allowed height from the first valid block
    if (total_rows == -1) {
        total_rows = NF  # Number of fields (lines) is the total rows
        # The available space for pins/key cuts is total rows minus the top and bottom marker rows
        max_h_allowed = total_rows - 2
        if (max_h_allowed < 0) {
             print "Error: Invalid schematic height detected (" total_rows " rows)." > "/dev/stderr"
             exit 1
        }
    } else if (NF != total_rows) {
        # Optional: Warn if subsequent blocks have different heights, though problem implies consistency
        # print "Warning: Inconsistent schematic height found at record", NR > "/dev/stderr"
    }


    # Identify if the current block is a lock or a key
    first_row = $1
    last_row = $NF

    # Lock: Top row is #####, Bottom row is .....
    is_lock = (first_row ~ /^#####$/ && last_row ~ /^\.\.\.\.\.$/)
    # Key: Top row is ....., Bottom row is #####
    is_key = (first_row ~ /^\.\.\.\.\.$/ && last_row ~ /^#####$/)

    # If it's neither a lock nor a key, skip this block
    if (!is_lock && !is_key) {
        # Optional: print "Warning: Skipping unrecognized block format at record", NR > "/dev/stderr"
        next
    }

    # Calculate the heights for each of the 5 columns
    if (is_lock) {
        lock_idx = lock_count++
        for (col = 1; col <= num_cols; col++) {
            h = 0
            # Calculate lock pin height: count '#' downwards from the top row (index 1)
            # until a '.' is encountered.
            for (row = 1; row <= total_rows; row++) {
                if (substr($row, col, 1) == "#") {
                    h++
                } else {
                    break # Stop at the first '.'
                }
            }
            # The actual pin height is the count minus 1 (excluding the top marker row)
            locks[lock_idx, col-1] = h - 1 # Store using 0-based column index
        }
    } else { # is_key
        key_idx = key_count++
        for (col = 1; col <= num_cols; col++) {
            h = 0
            # Calculate key cut height: count '#' upwards from the bottom row (index total_rows)
            # until a '.' is encountered.
            for (row = total_rows; row >= 1; row--) {
                 if (substr($row, col, 1) == "#") {
                    h++
                } else {
                    break # Stop at the first '.'
                }
            }
            # The actual key height is the count minus 1 (excluding the bottom marker row)
            keys[key_idx, col-1] = h - 1 # Store using 0-based column index
        }
    }
}

# END block: Executed after all records have been processed
END {
    # Perform sanity checks before comparison
    if (max_h_allowed < 0) {
         print "Error: Could not determine schematic dimensions from input." > "/dev/stderr"
         exit 1
    }
     if (lock_count == 0) {
         print "Warning: No valid lock schematics found." > "/dev/stderr"
     }
     if (key_count == 0) {
          print "Warning: No valid key schematics found." > "/dev/stderr"
     }

    # Compare every lock with every key
    for (l = 0; l < lock_count; l++) {
        for (k = 0; k < key_count; k++) {
            fits = 1 # Assume this pair fits initially

            # Check each column for overlap
            for (c = 0; c < num_cols; c++) {
                lock_h = locks[l, c]
                key_h = keys[k, c]

                # If the sum of heights in any column exceeds the allowed space, they don't fit
                if (lock_h + key_h > max_h_allowed) {
                    fits = 0
                    break # No need to check other columns for this pair
                }
            }

            # If all columns passed the check, increment the fit count
            if (fits) {
                fit_count++
            }
        }
    }

    # Print the final count of fitting lock/key pairs
    print fit_count
}
