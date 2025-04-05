
#!/bin/bash

# --- Day 5: If You Give A Seed A Fertilizer ---
# Bash solution using awk for efficient range processing.

# Function to encapsulate the main logic
main() {
    local input_file="input.txt"

    if [[ ! -f "$input_file" ]]; then
        echo "Error: Input file '$input_file' not found." >&2
        exit 1
    fi

    # Use awk to process the file and ranges
    # awk is well-suited for parsing the structured input and handling arithmetic on ranges.
    awk '
    BEGIN {
        # Initialize minimum location to a very large number
        min_location = -1
        num_ranges = 0
        num_rules = 0
        processing_map = 0 # Flag to indicate if we are reading map rules
    }

    # --- Parsing ---

    # Line 1: Seeds
    NR == 1 {
        # Part 2: Parse seed ranges (start, length)
        for (i = 2; i <= NF; i += 2) {
            start = $i
            len = $(i+1)
            # Store ranges as start and end (exclusive)
            ranges[num_ranges, "start"] = start
            ranges[num_ranges, "end"] = start + len
            num_ranges++
        }
        next # Skip to next line
    }

    # Empty line: signifies end of previous map section OR start before first map
    NF == 0 {
        if (processing_map) {
            # Process the map we just finished reading rules for
            apply_map()
            processing_map = 0 # Reset flag
            num_rules = 0     # Reset rules for the next map
        }
        next # Skip empty lines
    }

    # Map header line (e.g., "seed-to-soil map:")
    /map:/ {
        processing_map = 1 # Start processing rules for this map
        next
    }

    # Map rule line (destination_start source_start length)
    processing_map && NF == 3 {
        dest_start = $1
        src_start = $2
        len = $3
        rules[num_rules, "dest"] = dest_start
        rules[num_rules, "src"] = src_start
        rules[num_rules, "len"] = len
        num_rules++
        next
    }

    # --- Range Transformation Logic ---

    # Function to apply the current set of rules to the current ranges
    function apply_map() {
        num_next_ranges = 0 # Counter for the output ranges of this stage

        # For each input range, apply all rules to potentially split and map it
        for (i = 0; i < num_ranges; i++) {
            # Ranges still needing processing for this input range i
            # Store as start/end pairs in a temporary array-like structure
            # Awk lacks easy list-of-lists, so simulate with indexed vars
            num_remaining = 1
            remaining[0, "start"] = ranges[i, "start"]
            remaining[0, "end"] = ranges[i, "end"]

            # Array to store parts of range i that have been mapped by a rule
            num_mapped_this_rule_pass = 0

            # Apply each rule to the remaining parts of the current input range
            for (r = 0; r < num_rules; r++) {
                rule_src = rules[r, "src"]
                rule_dest = rules[r, "dest"]
                rule_len = rules[r, "len"]
                rule_src_end = rule_src + rule_len
                rule_offset = rule_dest - rule_src

                num_unmapped_next = 0 # Build the next set of remaining ranges

                # Check each remaining chunk against the current rule
                for (rem_idx = 0; rem_idx < num_remaining; rem_idx++) {
                    rem_start = remaining[rem_idx, "start"]
                    rem_end = remaining[rem_idx, "end"]

                    # Calculate overlap between remaining chunk and rule source range
                    overlap_start = (rem_start > rule_src) ? rem_start : rule_src
                    overlap_end = (rem_end < rule_src_end) ? rem_end : rule_src_end

                    # If there is a valid overlap
                    if (overlap_start < overlap_end) {
                        # 1. Add the mapped overlapping part to the next ranges
                        next_ranges[num_next_ranges, "start"] = overlap_start + rule_offset
                        next_ranges[num_next_ranges, "end"] = overlap_end + rule_offset
                        num_next_ranges++

                        # 2. Add the part *before* the overlap (if any) back to remaining for future rules
                        if (rem_start < overlap_start) {
                             unmapped_next[num_unmapped_next, "start"] = rem_start
                             unmapped_next[num_unmapped_next, "end"] = overlap_start
                             num_unmapped_next++
                        }
                         # 3. Add the part *after* the overlap (if any) back to remaining for future rules
                        if (overlap_end < rem_end) {
                             unmapped_next[num_unmapped_next, "start"] = overlap_end
                             unmapped_next[num_unmapped_next, "end"] = rem_end
                             num_unmapped_next++
                        }
                    } else {
                        # No overlap, keep the whole remaining chunk for the next rule
                        unmapped_next[num_unmapped_next, "start"] = rem_start
                        unmapped_next[num_unmapped_next, "end"] = rem_end
                        num_unmapped_next++
                    }
                }
                # Update remaining array for the next rule
                 num_remaining = num_unmapped_next
                 for(k=0; k<num_remaining; ++k) {
                     remaining[k, "start"] = unmapped_next[k, "start"]
                     remaining[k, "end"] = unmapped_next[k, "end"]
                 }

                 # Optimization: if no chunks left, break early from rule loop
                 if (num_remaining == 0) break
            }

            # After checking all rules, any remaining chunks are identity-mapped
            # Add them to the next_ranges
            for (rem_idx = 0; rem_idx < num_remaining; rem_idx++) {
                next_ranges[num_next_ranges, "start"] = remaining[rem_idx, "start"]
                next_ranges[num_next_ranges, "end"] = remaining[rem_idx, "end"]
                num_next_ranges++
            }
        }

        # Update main ranges array for the next stage
        num_ranges = num_next_ranges
        delete ranges # Clear old ranges
        for (j = 0; j < num_ranges; j++) {
            ranges[j, "start"] = next_ranges[j, "start"]
            ranges[j, "end"] = next_ranges[j, "end"]
        }
        delete next_ranges # Clear temporary array
        # Keep rules array populated until explicitly cleared when next map starts
    }

    # --- End of Processing ---

    END {
        # Apply the last map transformation (since processing happens at the start of the *next* map or empty line)
        if (num_rules > 0) {
             apply_map()
        }

        # Find the minimum start location from the final ranges
        min_location = -1
        if (num_ranges > 0) {
            min_location = ranges[0, "start"]
            for (i = 1; i < num_ranges; i++) {
                if (ranges[i, "start"] < min_location) {
                    min_location = ranges[i, "start"]
                }
            }
        } else {
             # Handle case where no seeds or ranges resulted
             print "Error: No final location ranges found." > "/dev/stderr"
             exit 1
        }

        # Print the final minimum location
        print min_location
    }
    ' "$input_file" # Pass the input file to awk
}

# Call the main function
main

