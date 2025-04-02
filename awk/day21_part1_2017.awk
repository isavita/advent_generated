
#!/usr/bin/awk -f

# --- Day 21: Fractal Art ---
# AWK solution

# --- Helper Functions ---

# Function to split a pattern string ("r1/r2/...") into a 2D array representation
# arr is passed by reference (associative array used as 2D)
# Returns the size (n) of the pattern
function split_pattern(pattern, arr,    n, rows, i, j) {
    delete arr # Clear the array before filling
    n = split(pattern, rows, "/")
    for (i = 1; i <= n; i++) {
        for (j = 1; j <= n; j++) {
            arr[i-1, j-1] = substr(rows[i], j, 1)
        }
    }
    return n
}

# Function to join a 2D array representation back into a pattern string
function join_pattern(arr, size,    i, j, row_str, result) {
    result = ""
    for (i = 0; i < size; i++) {
        row_str = ""
        for (j = 0; j < size; j++) {
            # Handle potential unset indices (though should not happen with correct usage)
            if (!((i,j) in arr)) arr[i,j]="." # Default to off if missing, defensive
            row_str = row_str arr[i, j]
        }
        result = result (i > 0 ? "/" : "") row_str
    }
    return result
}

# Function to rotate a pattern string 90 degrees clockwise
function rotate(pattern,   size, temp_arr, rotated_arr, i, j) {
    size = split_pattern(pattern, temp_arr)
    for (i = 0; i < size; i++) {
        for (j = 0; j < size; j++) {
            rotated_arr[j, size - 1 - i] = temp_arr[i, j]
        }
    }
    return join_pattern(rotated_arr, size)
}

# Function to flip a pattern string horizontally
function flip(pattern,   size, temp_arr, flipped_arr, i, j) {
    size = split_pattern(pattern, temp_arr)
    for (i = 0; i < size; i++) {
        for (j = 0; j < size; j++) {
            flipped_arr[i, size - 1 - j] = temp_arr[i, j]
        }
    }
    return join_pattern(flipped_arr, size)
}

# --- Main Program Logic (within BEGIN block) ---
BEGIN {
    # Use explicit input file reading
    INPUT_FILE = "input.txt"
    ITERATIONS = 5

    # Store rules: rules[input_pattern_variation] = output_pattern
    FS = " => " # Set field separator for reading rules

    # Read rules from input.txt and store all 8 variations
    while ((getline < INPUT_FILE) > 0) {
        if ($0 ~ /=>/) { # Basic check for valid rule format
            input_pat = $1
            output_pat = $2
            current = input_pat

            # Generate rotations and flips, store output for each
            for (rot = 0; rot < 4; rot++) {
                rules[current] = output_pat         # Store original/rotated
                rules[flip(current)] = output_pat   # Store flipped version
                current = rotate(current)           # Rotate for next iteration
            }
        }
    }
    close(INPUT_FILE) # Important to close the file

    # --- Grid Initialization ---
    # Initial pattern: .#./..#/###
    grid[0, 0] = "." ; grid[0, 1] = "#" ; grid[0, 2] = "."
    grid[1, 0] = "." ; grid[1, 1] = "." ; grid[1, 2] = "#"
    grid[2, 0] = "#" ; grid[2, 1] = "#" ; grid[2, 2] = "#"
    N = 3 # Initial grid size

    # --- Iteration Loop ---
    for (iter = 1; iter <= ITERATIONS; iter++) {
        # Determine split size (k) and output square size (m)
        if (N % 2 == 0) {
            k = 2 # Split into 2x2
            m = 3 # Replace with 3x3
        } else { # N % 3 == 0
            k = 3 # Split into 3x3
            m = 4 # Replace with 4x4
        }

        new_N = (N / k) * m # Calculate new grid size
        delete new_grid      # Clear the new grid before population

        # Iterate through the blocks of the current grid
        for (block_row = 0; block_row < N / k; block_row++) {
            for (block_col = 0; block_col < N / k; block_col++) {

                # 1. Extract the kxk subgrid into a pattern string
                sub_pattern = ""
                start_row = block_row * k
                start_col = block_col * k
                for (r = 0; r < k; r++) {
                    row_str = ""
                    for (c = 0; c < k; c++) {
                        row_str = row_str grid[start_row + r, start_col + c]
                    }
                    sub_pattern = sub_pattern (r > 0 ? "/" : "") row_str
                }

                # 2. Find the corresponding enhancement rule (using pre-computed variations)
                if (!(sub_pattern in rules)) {
                    print "FATAL ERROR: No rule found for pattern: " sub_pattern > "/dev/stderr"
                    exit 1
                }
                output_pattern = rules[sub_pattern]

                # 3. Split the output pattern into a temporary 2D array
                split_pattern(output_pattern, output_arr) # size m x m

                # 4. Place the mxm output block into the new_grid
                new_start_row = block_row * m
                new_start_col = block_col * m
                for (r = 0; r < m; r++) {
                    for (c = 0; c < m; c++) {
                        new_grid[new_start_row + r, new_start_col + c] = output_arr[r, c]
                    }
                }
            }
        }

        # 5. Update the main grid for the next iteration
        delete grid # Clear old grid
        # Copy new_grid to grid (AWK requires element-by-element copy for associative arrays)
        for (key in new_grid) {
            grid[key] = new_grid[key]
        }
        N = new_N # Update grid size
    }

    # --- Final Count ---
    on_count = 0
    # Iterate through the final grid (using stored size N)
    # Note: Iterating `for (key in grid)` is less reliable if grid might be sparse
    for (y = 0; y < N; y++) {
        for (x = 0; x < N; x++) {
             # Check if the index exists and equals "#"
             if ((y,x) in grid && grid[y, x] == "#") {
                on_count++
            }
        }
    }

    print on_count # Print the final result
}

# No END block needed as all processing happens in BEGIN
# No default file processing needed either
