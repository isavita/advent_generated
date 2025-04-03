
# AWK script for simulating falling rocks with jet patterns and cycle detection

# Function to check if a rock can move by (dx, dy)
# Uses global vars: rock_x, rock_y, num_points, chamber
# Declares local vars i, new_x, new_y after parameters
function can_move(dx, dy,    i, new_x, new_y) {
    for (i = 0; i < num_points; i++) {
        new_x = rock_x[i] + dx
        new_y = rock_y[i] + dy
        # Check boundaries (0-6 width, y > 0 floor)
        if (new_x < 0 || new_x > 6 || new_y < 1) return 0 # Collision with wall or floor
        # Check collision with settled rocks
        if ((new_x, new_y) in chamber) return 0 # Collision with another rock
    }
    return 1 # No collision, move is possible
}

# Function to get the profile of the top 'depth' rows of the chamber
# Used for cycle detection. Returns a string representation of relative heights.
# Uses global vars: highest_y, chamber
# Declares local vars x, y, profile_str, found, rel_y, limit_y after parameters
function get_chamber_profile(depth,    x, y, profile_str, found, rel_y, limit_y) {
    profile_str = ""
    for (x = 0; x < 7; x++) {
        found = 0
        # Limit search to 'depth' rows down from highest_y, but not below y=1
        limit_y = highest_y - depth + 1
        if (limit_y < 1) limit_y = 1
        for (y = highest_y; y >= limit_y; y--) {
            if ((x, y) in chamber) {
                rel_y = highest_y - y # Height relative to current max height
                profile_str = profile_str (profile_str == "" ? "" : "|") rel_y
                found = 1
                break # Found highest point in this column within depth
            }
        }
        if (!found) {
            # If no rock found within depth, use depth as placeholder value
            profile_str = profile_str (profile_str == "" ? "" : "|") depth
        }
    }
    return profile_str
}

# Main logic starts here, executed before processing any input lines
BEGIN {
    # Read the single line of jet pattern from input.txt
    if (getline jet_pattern < "input.txt" <= 0) {
        print "Error: Cannot read input.txt" > "/dev/stderr"
        exit 1
    }
    close("input.txt")

    total_rocks = 1000000000000
    jet_len = length(jet_pattern)
    profile_depth = 50 # How deep to look for the chamber profile (for cycle detection)

    # Define the 5 rock shapes using relative coordinates (dx, dy) from bottom-left
    # Shape 0: - (Horizontal Line)
    shape_len[0] = 4; shape_dx[0,0]=0; shape_dy[0,0]=0; shape_dx[0,1]=1; shape_dy[0,1]=0; shape_dx[0,2]=2; shape_dy[0,2]=0; shape_dx[0,3]=3; shape_dy[0,3]=0;
    # Shape 1: + (Plus)
    shape_len[1] = 5; shape_dx[1,0]=1; shape_dy[1,0]=0; shape_dx[1,1]=0; shape_dy[1,1]=1; shape_dx[1,2]=1; shape_dy[1,2]=1; shape_dx[1,3]=2; shape_dy[1,3]=1; shape_dx[1,4]=1; shape_dy[1,4]=2;
    # Shape 2: L (Inverted L)
    shape_len[2] = 5; shape_dx[2,0]=0; shape_dy[2,0]=0; shape_dx[2,1]=1; shape_dy[2,1]=0; shape_dx[2,2]=2; shape_dy[2,2]=0; shape_dx[2,3]=2; shape_dy[2,3]=1; shape_dx[2,4]=2; shape_dy[2,4]=2;
    # Shape 3: | (Vertical Line)
    shape_len[3] = 4; shape_dx[3,0]=0; shape_dy[3,0]=0; shape_dx[3,1]=0; shape_dy[3,1]=1; shape_dx[3,2]=0; shape_dy[3,2]=2; shape_dx[3,3]=0; shape_dy[3,3]=3;
    # Shape 4: Square
    shape_len[4] = 4; shape_dx[4,0]=0; shape_dy[4,0]=0; shape_dx[4,1]=1; shape_dy[4,1]=0; shape_dx[4,2]=0; shape_dy[4,2]=1; shape_dx[4,3]=1; shape_dy[4,3]=1;

    # Initialize chamber with the floor at y=0
    highest_y = 0
    for (x = 0; x < 7; x++) {
        chamber[x, 0] = 1
    }

    # Simulation state variables
    jet_index = 0       # Current position in the jet pattern
    rock_index = 0      # Current rock shape index
    rock_number = 0     # Number of rocks fallen so far
    additional_height = 0 # Height added by fast-forwarding cycles

    # Cycle detection state: seen_states[state_key] = rock_number SUBSEP highest_y
    # delete seen_states # AWK arrays start empty

    # Main simulation loop
    while (rock_number < total_rocks) {
        # Determine current rock shape and its number of points
        current_rock_type = rock_index % 5
        num_points = shape_len[current_rock_type]

        # Calculate starting position for the new rock
        start_x = 2
        start_y = highest_y + 4

        # Initialize current rock's coordinates (use temp arrays rock_x, rock_y)
        delete rock_x; delete rock_y
        for (i = 0; i < num_points; i++) {
            rock_x[i] = start_x + shape_dx[current_rock_type, i]
            rock_y[i] = start_y + shape_dy[current_rock_type, i]
        }

        # Rock falling loop
        while (1) {
            # 1. Apply jet push
            jet_dir = substr(jet_pattern, (jet_index % jet_len) + 1, 1) # AWK strings are 1-indexed
            jet_index++
            push_dx = (jet_dir == ">" ? 1 : -1)

            if (can_move(push_dx, 0)) { # Check if move is possible
                for (i = 0; i < num_points; i++) rock_x[i] += push_dx # Apply move
            }

            # 2. Attempt to move down
            if (can_move(0, -1)) { # Check if move down is possible
                for (i = 0; i < num_points; i++) rock_y[i] -= 1 # Apply move down
            } else {
                # Rock comes to rest
                for (i = 0; i < num_points; i++) {
                    x = rock_x[i]; y = rock_y[i]
                    chamber[x, y] = 1 # Add rock part to chamber
                    if (y > highest_y) highest_y = y # Update max height
                }
                break # Exit falling loop, move to next rock
            }
        }

        # Cycle detection logic
        profile_str = get_chamber_profile(profile_depth)
        # State key: rock type index, jet pattern index, chamber profile string
        state_key = (rock_index % 5) SUBSEP (jet_index % jet_len) SUBSEP profile_str

        if (state_key in seen_states) {
            # Cycle detected!
            split(seen_states[state_key], prev_data, SUBSEP)
            prev_rock = prev_data[1]
            prev_height = prev_data[2]

            cycle_len_rocks = rock_number - prev_rock
            cycle_len_height = highest_y - prev_height

            if (cycle_len_rocks > 0) {
                remaining_rocks = total_rocks - rock_number
                num_cycles = int(remaining_rocks / cycle_len_rocks) # Integer division

                # Fast-forward simulation
                additional_height += num_cycles * cycle_len_height
                rock_number += num_cycles * cycle_len_rocks
                # After fast-forwarding, clear seen_states to avoid re-detecting
                # the same cycle immediately if the state repeats right after jump.
                # This prevents potential infinite loops if cycle_len_rocks is small.
                delete seen_states
            } else {
                 # Handle edge case: state repeats immediately (cycle_len=0) - unlikely
                 # Update the state info without fast-forwarding
                 seen_states[state_key] = rock_number SUBSEP highest_y
            }

        } else {
            # Store current state for future cycle detection
            seen_states[state_key] = rock_number SUBSEP highest_y
        }

        # Move to the next rock (if we haven't already reached the total via cycle jump)
         if (rock_number < total_rocks) {
             rock_number++
         }
        rock_index++

        # Optional: Prune chamber to save memory, might not be needed due to fast cycle detection
        # if (rock_number % 5000 == 0) { # Prune periodically
        #     prune_limit = highest_y - profile_depth - 20 # Keep profile depth + buffer
        #     for (coord in chamber) {
        #         split(coord, xy, SUBSEP)
        #         if (xy[2] < prune_limit && xy[2] > 0) { # Dont prune floor (y=0)
        #              delete chamber[coord]
        #         }
        #     }
        # }
    }

    # Print the final calculated height
    # Use "%.0f" format specifier to ensure integer output for large numbers
    printf "%.0f\n", highest_y + additional_height
}

