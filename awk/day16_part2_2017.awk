
#!/usr/bin/awk -f

# --- Day 16: Permutation Promenade ---
# AWK solution

# Function to convert the progs array to a string representation
function get_state_string(progs,   i, s) {
    s = ""
    for (i = 0; i < num_progs; i++) {
        s = s progs[i]
    }
    return s
}

# Function to perform one full round of dance moves
function perform_dance(moves, progs, pos,   m, move, type, parts, val, p1, p2, idx1, idx2, tmp, i, j, k, new_progs) {
    # moves: array of dance moves
    # progs: array mapping position index to program name (0-indexed)
    # pos:   associative array mapping program name to position index

    for (m = 1; m <= n_moves; m++) {
        move = moves[m]
        type = substr(move, 1, 1)
        val = substr(move, 2)

        if (type == "s") {
            # Spin sX: Move X programs from end to front
            val = val + 0 # Convert to number
            # Create a temporary array for the new order
            delete new_progs
            # Copy last 'val' elements to the start of new_progs
            for (i = 0; i < val; i++) {
                new_progs[i] = progs[num_progs - val + i]
            }
            # Copy first 'num_progs - val' elements to the end of new_progs
            for (i = 0; i < num_progs - val; i++) {
                new_progs[val + i] = progs[i]
            }
            # Update the main progs array and the pos map
            for (i = 0; i < num_progs; i++) {
                progs[i] = new_progs[i]
                pos[progs[i]] = i
            }
        } else if (type == "x") {
            # Exchange xA/B: Swap programs at positions A and B
            split(val, parts, "/")
            idx1 = parts[1] + 0
            idx2 = parts[2] + 0
            # Get program names at these positions
            p1 = progs[idx1]
            p2 = progs[idx2]
            # Swap in progs array
            tmp = progs[idx1]
            progs[idx1] = progs[idx2]
            progs[idx2] = tmp
            # Update positions in pos map
            pos[p1] = idx2
            pos[p2] = idx1
        } else if (type == "p") {
            # Partner pA/B: Swap programs named A and B
            split(val, parts, "/")
            p1 = parts[1]
            p2 = parts[2]
            # Get positions of these programs
            idx1 = pos[p1]
            idx2 = pos[p2]
            # Swap in progs array (using indices)
            tmp = progs[idx1]
            progs[idx1] = progs[idx2]
            progs[idx2] = tmp
            # Update positions in pos map (swapping indices for the names)
            pos[p1] = idx2
            pos[p2] = idx1
        }
    }
}

BEGIN {
    # --- Configuration ---
    num_progs = 16
    total_dances = 1000000000
    input_file = "input.txt"

    # --- Initialization ---
    # Initialize program line: progs[0]="a", progs[1]="b", ...
    # Initialize position map: pos["a"]=0, pos["b"]=1, ...
    for (i = 0; i < num_progs; i++) {
        char = sprintf("%c", 97 + i) # 97 is ASCII for 'a'
        progs[i] = char
        pos[char] = i
    }

    # Read the single line of dance moves from input.txt
    if ((getline dance_line < input_file) <= 0) {
        print "Error: Could not read input file or file is empty." > "/dev/stderr"
        exit 1
    }
    close(input_file)

    # Split the moves into an array
    n_moves = split(dance_line, moves, ",")

    # --- Part 1 ---
    # Perform one dance
    perform_dance(moves, progs, pos)
    part1_result = get_state_string(progs)
    print "Part 1:", part1_result

    # --- Part 2 ---
    # Reset programs to the state *after* the first dance (already done)
    # We need to perform total_dances - 1 more dances starting from part1_result state
    # Look for cycles

    # Store seen states: seen_states[state_string] = dance_number
    delete seen_states
    seen_states[get_state_string(progs)] = 1 # Record state after dance 1

    dance_num = 1 # We already performed the first dance

    while (dance_num < total_dances) {
        dance_num++
        perform_dance(moves, progs, pos)
        current_state = get_state_string(progs)

        if (current_state in seen_states) {
            # Cycle detected!
            prev_dance_num = seen_states[current_state]
            cycle_len = dance_num - prev_dance_num
            remaining_dances = total_dances - dance_num

            # Calculate how many steps into the cycle the final state will be
            effective_remaining = remaining_dances % cycle_len

            # Perform the remaining 'effective_remaining' dances
            for (i = 1; i <= effective_remaining; i++) {
                perform_dance(moves, progs, pos)
            }

            # We have reached the final state, break the loop
            dance_num = total_dances # Set to target to exit loop
        } else {
            # Store the new state
            seen_states[current_state] = dance_num
        }
    }

    # Print Part 2 result
    part2_result = get_state_string(progs)
    print "Part 2:", part2_result

    # Exit cleanly (optional, AWK exits automatically after BEGIN if no input patterns)
    exit 0
}

# No main processing block needed as everything is done in BEGIN
# No END block needed
