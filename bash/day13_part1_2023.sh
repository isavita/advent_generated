
#!/bin/bash

# --- Day 13: Point of Incidence ---
# Part 1: Find the single reflection line (horizontal or vertical)
# Reads from input.txt and prints the total summary value to stdout.
# Uses awk for efficient pattern processing.

INPUT_FILE="input.txt"

# Main function to encapsulate the script logic
main() {
    # Check if the input file exists
    if [[ ! -f "$INPUT_FILE" ]]; then
        echo "Error: Input file '$INPUT_FILE' not found." >&2
        exit 1
    fi

    # Use awk to process the input file
    # RS="" treats blocks separated by blank lines as records
    # FS="\n" treats each line within a block as a field
    # The awk script calculates the summary for each pattern and sums them up.
    awk '
    # Set record separator to blank lines and field separator to newline
    BEGIN { RS = ""; FS = "\n"; total_sum = 0 }

    # Function to check for reflection in an array `a` with `n` elements (rows or columns).
    # Elements are indexed from 1.
    # `is_horizontal` is a flag: 1 for horizontal check (rows), 0 for vertical (columns, using transposed data).
    # Returns the score (100 * rows_above or cols_left) if reflection found, otherwise 0.
    function check_reflection(a, n, is_horizontal,    r, k, up_idx, down_idx, is_reflection, multiplier, score) {
        multiplier = is_horizontal ? 100 : 1 # Score multiplier based on orientation

        # Iterate through possible reflection lines (line is BETWEEN r and r+1)
        for (r = 1; r < n; r++) {
            is_reflection = 1 # Assume it reflects perfectly until proven otherwise
            # Check pairs outwards from the potential reflection line
            for (k = 0; ; k++) {
                up_idx = r - k       # Index of element above/left
                down_idx = r + 1 + k # Index of element below/right

                # Check if indices are out of bounds
                if (up_idx < 1 || down_idx > n) {
                    break # Reached the edge(s), all checked pairs matched.
                }

                # Compare the elements (rows or transposed columns)
                if (a[up_idx] != a[down_idx]) {
                    is_reflection = 0 # Mismatch found
                    break
                }
            }

            # If the inner loop completed without finding a mismatch
            if (is_reflection) {
                score = multiplier * r # Calculate score (r = rows above or columns left)
                return score           # Return the calculated score
            }
        }
        return 0 # No reflection found for this orientation
    }

    # Process each pattern (record)
    {
        # Store lines in an array `lines` (awk uses 1-based indexing)
        num_rows = NF # NF is the number of fields (lines) in the current record
        delete lines # Clear array from previous pattern
        for (i = 1; i <= num_rows; i++) {
            lines[i] = $i # $i is the i-th field (line)
        }
        # Handle potential trailing newline creating an empty field if input ends with blank line(s)
        if (num_rows > 0 && lines[num_rows] == "") {
             num_rows--
        }
        if (num_rows == 0) next # Skip empty blocks if any


        num_cols = length(lines[1]) # Get number of columns from the first line

        # 1. Check for horizontal reflection
        h_score = check_reflection(lines, num_rows, 1) # 1 = check horizontal
        if (h_score > 0) {
            total_sum += h_score
            next # Found reflection, move to the next pattern/record
        }

        # 2. If no horizontal, check for vertical reflection
        # Transpose the pattern: columns become elements in `transposed_lines`
        delete transposed_lines
        for (j = 1; j <= num_cols; j++) { # j iterates through columns
            col_str = ""
            for (i = 1; i <= num_rows; i++) { # i iterates through rows
                col_str = col_str substr(lines[i], j, 1) # Build column string
            }
            transposed_lines[j] = col_str
        }

        # Check for reflection in the transposed data (effectively checking columns)
        v_score = check_reflection(transposed_lines, num_cols, 0) # 0 = check vertical
        if (v_score > 0) {
             total_sum += v_score
             next # Found reflection, move to the next pattern/record
        }

        # According to the problem, every pattern should have exactly one line of reflection.
        # If we reach here, something is unexpected.
        # print "Warning: No reflection found for pattern:" > "/dev/stderr"
        # for(i=1; i<=num_rows; i++) print lines[i] > "/dev/stderr"
    }

    # After processing all records
    END { print total_sum }

    ' "$INPUT_FILE" # Pass the input file to awk
}

# Execute the main function
main
