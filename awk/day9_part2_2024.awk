
BEGIN {
    # Initialize variables
    disk_len = 0
    file_id_counter = 0
    is_file = 1 # 1 for file, 0 for free space
    file_count = 0
    checksum = 0
    getline line < "input.txt" # Read the single line from input.txt

    # 1. Expand disk layout from the input line
    for (i = 1; i <= length(line); ++i) {
        len = substr(line, i, 1) + 0 # Get length, ensure numeric
        for (j = 0; j < len; ++j) {
            if (is_file) {
                disk[disk_len] = file_id_counter # Store file ID
            } else {
                disk[disk_len] = "."          # Store free space marker
            }
            disk_len++
        }
        if (is_file) {
            file_id_counter++ # Increment file ID for the next file block
        }
        is_file = 1 - is_file # Toggle between file and free space
    }

    # 2. Build file segments list (using parallel arrays)
    curr_id = -1 # Marker for not currently inside a file segment
    start = 0
    for (i = 0; i < disk_len; ++i) {
        val = disk[i]
        # Check if we are leaving a file segment
        if (val == "." && curr_id != -1) {
             files_id[file_count] = curr_id
             files_start[file_count] = start
             files_end[file_count] = i - 1
             file_count++
             curr_id = -1 # Reset current file ID tracker
        # Check if we are entering a new file segment or continuing one
        } else if (val != ".") {
            file_id_val = val + 0 # Ensure numeric file ID
            # If this is a different file ID than the one we were tracking
            if (file_id_val != curr_id) {
                # If we were tracking a previous file, record its end
                if (curr_id != -1) {
                    files_id[file_count] = curr_id
                    files_start[file_count] = start
                    files_end[file_count] = i - 1
                    file_count++
                }
                # Start tracking the new file segment
                curr_id = file_id_val
                start = i
            }
        }
    }
    # Handle file segment ending exactly at the end of the disk
    if (curr_id != -1) {
         files_id[file_count] = curr_id
         files_start[file_count] = start
         files_end[file_count] = disk_len - 1
         file_count++
    }

    # 3. Process files (in reverse order of discovery, matching Python's reversed list)
    for (f_idx = file_count - 1; f_idx >= 0; --f_idx) {
        current_id = files_id[f_idx]
        current_start = files_start[f_idx]
        current_end = files_end[f_idx]
        file_len = current_end - current_start + 1

        best_start_found = -1 # Index where the suitable free space starts
        span_len = 0
        current_span_start = -1

        # Find leftmost suitable free space block before the file's current start
        for (i = 0; i < current_start; ++i) {
            if (disk[i] == ".") {
                if (span_len == 0) {
                    current_span_start = i # Mark start of potential free block
                }
                span_len++
                if (span_len == file_len) {
                    best_start_found = current_span_start # Found exact size block
                    break # Stop searching, we want the leftmost one
                }
            } else {
                span_len = 0 # Reset free space counter
                current_span_start = -1
            }
        }

        # 4. Move file if suitable space was found
        if (best_start_found != -1) {
            # Clear the old location
            for (i = current_start; i <= current_end; ++i) {
                disk[i] = "."
            }
            # Copy file ID to the new location
            for (i = 0; i < file_len; ++i) {
                disk[best_start_found + i] = current_id
            }
        }
    }

    # 5. Calculate final checksum
    for (i = 0; i < disk_len; ++i) {
        if (disk[i] != ".") {
            checksum += i * (disk[i] + 0) # Ensure numeric value for calculation
        }
    }

    # Print the result
    print checksum
}
