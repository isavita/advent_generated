
using DataStructures # For Deque

# Define Keypad Layouts and Coordinate Maps

# K1: Numeric Keypad
# Mapping coordinates (row, column) to characters. (1,1) is top-left.
const K1_LAYOUT = Dict(
    (1,1)=>'7', (1,2)=>'8', (1,3)=>'9',
    (2,1)=>'4', (2,2)=>'5', (2,3)=>'6',
    (3,1)=>'1', (3,2)=>'2', (3,3)=>'3',
    (4,2)=>'0', (4,3)=>'A'
)
# Reverse mapping from character to coordinate
const K1_COORDS = Dict(v=>k for (k,v) in K1_LAYOUT)

# K2, K3: Directional Keypads
# Mapping coordinates (row, column) to characters. (1,1) is top-left, but (1,1) is a gap.
const K23_LAYOUT = Dict(
    (1,2)=>'^', (1,3)=>'A',
    (2,1)=>'<', (2,2)=>'v', (2,3)=>'>'
)
# Reverse mapping from character to coordinate
const K23_COORDS = Dict(v=>k for (k,v) in K23_LAYOUT)

# Check if a given coordinate is a valid button position on K1
is_valid_k1(pos) = haskey(K1_LAYOUT, pos)

# Check if a given coordinate is a valid button position on K2 or K3
is_valid_k23(pos) = haskey(K23_LAYOUT, pos)

# Calculate the potential next coordinate if a directional move is attempted from 'pos'
# This function *does not* check for validity on a specific keypad, only the coordinate change.
function try_move(pos, move)
    r, c = pos # Current row and column
    if move == '^'; return (r - 1, c); end # Move up
    if move == 'v'; return (r + 1, c); end # Move down
    if move == '<'; return (r, c - 1); end # Move left
    if move == '>'; return (r, c + 1); end # Move right
    # This should not be reached for valid directional moves, but included for completeness.
    return pos
end

# State transition function for the nested keypad system.
# Takes the current state (arm positions on K1, K2, K3) and the K3 input button pressed.
# Returns the next state (next arm positions), a potential output character, and whether a panic occurred.
function next_state_tuple(s_curr, m3)
    p1, p2, p3 = s_curr # Current positions of arms on K1, K2, K3

    if m3 in ['<', '>', '^', 'v']
        # Your arm moves on K3 based on your directional input m3
        p3_next = try_move(p3, m3)
        # The robot panics if its arm is aimed at a gap after a directional move.
        if !is_valid_k23(p3_next)
            # Moving your arm to a gap on K3 causes a panic at your level
            return (p1, p2, p3), nothing, true # Panic state, positions revert or freeze (conceptually)
        end
        # If valid, only your arm position changes. No output. No panic.
        return (p1, p2, p3_next), nothing, false # Valid move on K3
    elseif m3 == 'A'
        # You press 'A' on K3. Your arm is at p3. p3 must be a valid position on K3.
        # The button at p3 on K3 is pressed, providing input b3 to Robot 2.
        b3 = K23_LAYOUT[p3] # Input to Robot 2

        if b3 in ['<', '>', '^', 'v']
            # Robot 2 receives a directional input b3. Its arm moves on K2.
            p2_next = try_move(p2, b3)
            # Check if Robot 2's new position on K2 is valid.
            if !is_valid_k23(p2_next)
                # Robot 2 moving its arm to a gap on K2 causes panic
                return (p1, p2, p3), nothing, true # Panic state
            end
            # If valid, only Robot 2's arm position changes. No output. No panic.
            # Your arm and Robot 1's arm remain in their current positions.
            return (p1, p2_next, p3), nothing, false # Valid move on K2 by R2
        elseif b3 == 'A'
            # Robot 2 receives 'A' input. It presses the button at p2 on K2. p2 must be valid.
            # The button at p2 on K2 is pressed, providing input b2 to Robot 1.
            b2 = K23_LAYOUT[p2] # Input to Robot 1

            if b2 in ['<', '>', '^', 'v']
                # Robot 1 receives a directional input b2. Its arm moves on K1.
                p1_next = try_move(p1, b2)
                 # Check if Robot 1's new position on K1 is valid.
                 if !is_valid_k1(p1_next)
                     # Robot 1 moving its arm to a gap on K1 causes panic
                     return (p1, p2, p3), nothing, true # Panic state
                 end
                # If valid, only Robot 1's arm position changes. No output. No panic.
                # Your arm and Robot 2's arm remain in their current positions.
                return (p1_next, p2, p3), nothing, false # Valid move on K1 by R1
            elseif b2 == 'A'
                # Robot 1 receives 'A' input. It presses the button at p1 on K1. p1 must be valid.
                # This produces the final output character on the Numeric Keypad (K1).
                # If p1 were invalid here, a panic should theoretically have occurred
                # in a previous step involving Robot 1's directional movement.
                # We do a sanity check, but typically p1 is valid here if no prior panic occurred.
                if !is_valid_k1(p1)
                    return (p1, p2, p3), nothing, true # Panic (should be rare here)
                end
                b1 = K1_LAYOUT[p1] # Output character on K1
                # No arm positions change when Robot 1 successfully presses a button on K1.
                return (p1, p2, p3), b1, false # Output produced, no panic
            end
        end
    end
    # This part should not be reached with valid inputs and keypad layouts, indicates an issue.
    return (p1, p2, p3), nothing, true # Indicate unexpected state/panic
end


# The main function to solve the challenge
function solve()
    # Read all lines from the input file. Each line is a code sequence to type.
    file_path = "input.txt"
    codes = readlines(file_path)

    # Variable to accumulate the total complexity across all codes
    total_complexity = 0

    # Precompute the set of possible button presses on the K3 directional keypad that you can input.
    k3_moves = ['<', '>', '^', 'v', 'A']

    # Process each code provided in the input file one by one
    for code in codes
        # Extract the numeric part from the code string for complexity calculation.
        # Use filter to get only digits, then parse to Int. Handle cases with no digits (like "A").
        numeric_part_str = filter(isdigit, code)
        numeric_value = parse(Int, isempty(numeric_part_str) ? "0" : numeric_part_str)

        # Convert the code string into an array of characters. This allows easy indexing (1-based in Julia).
        target_code_chars = collect(code)

        # Define the initial state for the BFS search for this specific code.
        # The state is a tuple representing:
        # 1. Position of Robot 1's arm on K1 (coordinate tuple)
        # 2. Position of Robot 2's arm on K2 (coordinate tuple)
        # 3. Position of your arm on K3 (coordinate tuple)
        # 4. The number of characters from the target code that have been successfully typed so far (integer)
        # All arms start at the 'A' position on their respective keypads. Initially, 0 characters are typed.
        start_state = (K1_COORDS['A'], K23_COORDS['A'], K23_COORDS['A'], 0)

        # Setup for Breadth-First Search (BFS).
        # The queue stores tuples of (current_state, distance_from_start).
        # We use a Deque for efficient popping from the front.
        Q = Deque{Tuple{Tuple{Tuple{Int,Int}, Tuple{Int,Int}, Tuple{Int,Int}, Int}, Int}}()
        push!(Q, (start_state, 0)) # Start BFS from the initial state with distance 0

        # Dictionary to keep track of the shortest distance found to reach each state.
        # This acts as a visited set and stores the minimum number of K3 presses.
        Dist = Dict{Tuple{Tuple{Int,Int}, Tuple{Int,Int}, Tuple{Int,Int}, Int}, Int}(start_state => 0)

        # Variable to store the shortest number of K3 presses required to type the whole code.
        min_dist_code = -1

        # Start the BFS exploration process
        while !isempty(Q)
            # Get the state with the shortest distance from the queue (BFS property)
            curr_state, d_curr = popfirst!(Q)
            p1, p2, p3, typed_len = curr_state # Unpack components of the current state

            # Check if the entire target code sequence has been successfully typed in this state
            if typed_len == length(target_code_chars)
                # If we reached a state where all characters are typed, this is the goal state for this code.
                # Since BFS explores layer by layer, the first time we reach such a state gives the shortest path.
                min_dist_code = d_curr # The shortest distance (K3 presses) to reach this state
                break # Exit the BFS loop as the optimal path is found for this code
            end

            # Explore possible transitions from the current state by trying each possible K3 input button
            for m3 in k3_moves
                # Simulate the effect of pressing button m3 on K3, starting from the arm positions (p1, p2, p3).
                # The `next_state_tuple` function returns the resulting arm positions, any output character produced on K1,
                # and a boolean indicating if a panic occurred.
                (p1_next, p2_next, p3_next), output, panic = next_state_tuple((p1, p2, p3), m3)

                if panic
                    # If this sequence of button presses leads to a panic at any level, this path is invalid.
                    continue # Do not explore this branch further
                end

                # Determine the number of characters typed in the next state based on the output (if any)
                typed_len_next = typed_len # Assume no character is typed initially or correctly

                if output !== nothing # If this K3 move resulted in a character being typed on K1
                     # Check if the typed character is the *next* one expected in the target sequence.
                     # We must type the characters in order (e.g., type '0' then '2' for "029A").
                     # Check bounds: ensure we haven't already typed the whole code.
                     if typed_len < length(target_code_chars) && output == target_code_chars[typed_len + 1]
                         # The correct next character was typed successfully. Increment the count.
                         typed_len_next = typed_len + 1
                     else
                         # A character was typed, but it was not the correct next one in the sequence.
                         # Or, a character was typed after the sequence was already complete.
                         # In either case, this path does not lead to typing the target code correctly as a sequence.
                         # Ignore this path.
                         continue # Do not explore this branch further
                     end
                end

                # Define the next state for the BFS, including the updated arm positions and typed character count.
                s_next = (p1_next, p2_next, p3_next, typed_len_next)
                # The cost to reach this next state is the current distance plus 1 (for the single K3 button press).
                d_next = d_curr + 1

                # If this state has not been visited before, or if we found a shorter path to reach it
                if !haskey(Dist, s_next) || d_next < Dist[s_next]
                     # Record the shortest distance found to this state
                     Dist[s_next] = d_next
                     # Add the next state to the queue for further exploration
                     push!(Q, (s_next, d_next))
                end
            end # End of loop through possible K3 moves ('<', '>', '^', 'v', 'A')
        end # End of BFS while loop for the current code

        # After the BFS completes for a single code, min_dist_code holds the shortest
        # number of K3 presses required to type that entire code sequence.
        if min_dist_code == -1
            # This case indicates that the target code sequence is unreachable from the start state
            # given the keypad mechanics and panic rules. This shouldn't happen for valid problem inputs.
            println("Error: BFS failed to find a path to type code $code. It might be unreachable or the logic has an issue.")
            # Decide how to handle this error - printing a message and skipping the code is one option.
            continue # Skip calculation for this code and move to the next one
        end

        # The shortest sequence length of K3 presses required for this code is min_dist_code.
        # Calculate the complexity for the current code as (shortest K3 presses) * (numeric value).
        complexity = min_dist_code * numeric_value

        # Add the calculated complexity of this code to the running total complexity.
        total_complexity += complexity

        # Note: For each new code processed in the outer loop, the BFS starts from the initial state again.
        # The state (arm positions and typed progress) is code-specific.
    end # End of loop through all codes provided in the input file

    # After processing all codes, print the final accumulated total complexity to standard output.
    println(total_complexity)
end

# Main entry point function. This is where execution begins when the script is run.
function main()
    # Call the solve function which encapsulates the core logic of the problem
    solve()
end

# Execute the main function to start the program.
main()
