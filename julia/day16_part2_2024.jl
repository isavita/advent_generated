
#=
Solution for a maze problem involving movement and turning costs.
Part 1: Find the minimum cost path from 'S' to 'E'.
Part 2: Count the number of unique tiles on any minimum cost path.

Input is read from "input.txt".
Output is printed to standard output.

Usage: julia <script_name>.jl
=#

using DataStructures

# Constants for directions and costs
const EAST = 0
const SOUTH = 1
const WEST = 2
const NORTH = 3
# Julia is 1-based, map directions 0..3 to indices 1..4 for these arrays
const DR = [0, 1, 0, -1] # Change in row for direction index+1 (E, S, W, N)
const DC = [1, 0, -1, 0] # Change in column for direction index+1 (E, S, W, N)
const TURN_COST = 1000
const MOVE_COST = 1

# Type alias for state for clarity
const State = Tuple{Int, Int, Int} # (row, column, direction)

"""
    solve()

Reads the maze from "input.txt", calculates the minimum path cost (Part 1),
and counts the number of tiles on any optimal path (Part 2). Prints results.
Handles potential errors during file reading or processing.
"""
function solve()
    # --- Read Input ---
    local lines
    try
        lines = readlines("input.txt")
    catch e
        println(stderr, "Error reading input.txt: ", e)
        exit(1)
    end

    if isempty(lines)
        println(stderr, "Error: input.txt is empty.")
        exit(1)
    end

    # --- Parse Grid ---
    local grid
    try
        # Create grid with grid[r, c] indexing
        # hcat stacks vectors horizontally, collect converts string to char vector
        # permutedims transposes to get desired row, col indexing
        grid = permutedims(hcat(collect.(lines)...))
    catch e
        println(stderr, "Error parsing grid from input.txt. Ensure rectangular format: ", e)
        exit(1)
    end
    rows, cols = size(grid)

    start_pos = findfirst(==('S'), grid)
    end_pos = findfirst(==('E'), grid)

    # Validate start and end points exist
    if isnothing(start_pos)
        println(stderr, "Error: Start 'S' not found in input.")
        exit(1)
    end
     if isnothing(end_pos)
        println(stderr, "Error: End 'E' not found in input.")
        exit(1)
    end
    start_r, start_c = Tuple(start_pos)
    end_r, end_c = Tuple(end_pos)

    # Create a grid copy for logical checks, treating S/E as walkable floor '.'
    grid_logic = copy(grid)
    grid_logic[start_pos] = '.'
    grid_logic[end_pos] = '.'

    # --- Forward Dijkstra (Calculates minimum cost from Start to any State) ---
    dist_fwd = Dict{State, Int}() # State => Min Cost from Start
    pq_fwd = PriorityQueue{State, Int}() # Key: State, Value: Cost

    initial_state = (start_r, start_c, EAST) # Start at S, facing East
    dist_fwd[initial_state] = 0
    pq_fwd[initial_state] = 0 # Use update/insert syntax for PriorityQueue

    while !isempty(pq_fwd)
        state, cost = dequeue_pair!(pq_fwd)
        r, c, dir = state

        # If a shorter path to this state was already processed, skip
        if cost > get(dist_fwd, state, typemax(Int))
            continue
        end

        # Explore neighbors: Turn Left, Turn Right, Move Forward

        # Try Turning Left (Cost: TURN_COST)
        new_dir_left = (dir + 3) % 4 # Anti-clockwise
        new_cost_turn = cost + TURN_COST
        state_key_left = (r, c, new_dir_left)
        if new_cost_turn < get(dist_fwd, state_key_left, typemax(Int))
            dist_fwd[state_key_left] = new_cost_turn
            pq_fwd[state_key_left] = new_cost_turn # Update/insert into PQ
        end

        # Try Turning Right (Cost: TURN_COST)
        new_dir_right = (dir + 1) % 4 # Clockwise
        # new_cost_turn = cost + TURN_COST (same cost)
        state_key_right = (r, c, new_dir_right)
        if new_cost_turn < get(dist_fwd, state_key_right, typemax(Int))
            dist_fwd[state_key_right] = new_cost_turn
            pq_fwd[state_key_right] = new_cost_turn # Update/insert into PQ
        end

        # Try Moving Forward (Cost: MOVE_COST)
        # Use dir+1 for 1-based indexing into DR, DC arrays
        nr, nc = r + DR[dir+1], c + DC[dir+1]

        # Check grid bounds and if the next tile is not a wall '#'
        if 1 <= nr <= rows && 1 <= nc <= cols && grid_logic[nr, nc] != '#'
            new_cost_move = cost + MOVE_COST
            state_key_move = (nr, nc, dir) # Direction remains the same
            if new_cost_move < get(dist_fwd, state_key_move, typemax(Int))
                dist_fwd[state_key_move] = new_cost_move
                pq_fwd[state_key_move] = new_cost_move # Update/insert into PQ
            end
        end
    end

    # --- Part 1 Result ---
    min_total_cost = typemax(Int)
    # Check cost to reach the end tile 'E' from any final direction
    for d in 0:3
        cost_to_end_d = get(dist_fwd, (end_r, end_c, d), typemax(Int))
        min_total_cost = min(min_total_cost, cost_to_end_d)
    end

    # Handle case where the end tile is unreachable
    if min_total_cost == typemax(Int)
        println("Part 1: No path found.")
        println("Part 2: 0") # No path means 0 tiles on the optimal path
        return # Exit solve function
    end
    println("Part 1: ", min_total_cost)

    # --- Backward Dijkstra (Calculates minimum cost from any State TO End) ---
    dist_bwd = Dict{State, Int}() # State => Min Cost TO End
    pq_bwd = PriorityQueue{State, Int}() # Key: State, Value: Cost

    # Initialize backward search starting from all possible end states (end tile, any direction)
    for d in 0:3
        end_state = (end_r, end_c, d)
        dist_bwd[end_state] = 0 # Cost from end state to end is 0
        pq_bwd[end_state] = 0 # Use update/insert syntax
    end

    while !isempty(pq_bwd)
        state, cost = dequeue_pair!(pq_bwd) # cost = min cost from `state` TO end
        r, c, dir = state

        # If a shorter path from this state to the end was already processed, skip
        if cost > get(dist_bwd, state, typemax(Int))
            continue
        end

        # Explore states that could transition INTO the current `state`

        # 1. Came from Turning Left at (r, c): Previous state was (r, c, (dir+1)%4)
        prev_dir_turn_left = (dir + 1) % 4 # The direction before turning left
        prev_state_key_tl = (r, c, prev_dir_turn_left)
        # Cost from prev state = cost from current state + cost of the turn
        cost_from_prev_tl = cost + TURN_COST
        if cost_from_prev_tl < get(dist_bwd, prev_state_key_tl, typemax(Int))
             dist_bwd[prev_state_key_tl] = cost_from_prev_tl
             pq_bwd[prev_state_key_tl] = cost_from_prev_tl # Update/insert into PQ
        end

        # 2. Came from Turning Right at (r, c): Previous state was (r, c, (dir+3)%4)
        prev_dir_turn_right = (dir + 3) % 4 # The direction before turning right
        prev_state_key_tr = (r, c, prev_dir_turn_right)
        cost_from_prev_tr = cost + TURN_COST
        if cost_from_prev_tr < get(dist_bwd, prev_state_key_tr, typemax(Int))
             dist_bwd[prev_state_key_tr] = cost_from_prev_tr
             pq_bwd[prev_state_key_tr] = cost_from_prev_tr # Update/insert into PQ
        end

        # 3. Came from Moving Forward from (pr, pc): Previous state was (pr, pc, dir)
        # Calculate coordinates of the previous tile (pr, pc) by stepping backward
        # Use dir+1 for 1-based indexing into DR, DC arrays
        pr, pc = r - DR[dir+1], c - DC[dir+1]

        # Check if the previous tile is valid (within bounds and not a wall)
        if 1 <= pr <= rows && 1 <= pc <= cols && grid_logic[pr, pc] != '#'
             prev_state_key_move = (pr, pc, dir) # Direction is the same
             # Cost from prev state = cost from current state + cost of the move
             cost_from_prev_move = cost + MOVE_COST
             if cost_from_prev_move < get(dist_bwd, prev_state_key_move, typemax(Int))
                  dist_bwd[prev_state_key_move] = cost_from_prev_move
                  pq_bwd[prev_state_key_move] = cost_from_prev_move # Update/insert into PQ
             end
        end
    end

    # --- Part 2 Result: Count Unique Tiles on Optimal Paths ---
    optimal_tiles = Set{Tuple{Int, Int}}() # Store unique (row, col) tuples

    # Iterate through all states visited by the forward Dijkstra
    # A tile (r, c) is on an optimal path if there exists *any* direction 'dir' such that:
    # cost_from_start(r, c, dir) + cost_to_end(r, c, dir) == min_total_cost
    for (state_key, cost_f) in dist_fwd
        (r, c, _) = state_key # Extract tile coordinates

        # Optimization: If this tile is already confirmed optimal, skip further checks for it
        if (r, c) in optimal_tiles
             continue
        end

        # Get the minimum cost from this state to the end, using backward Dijkstra results
        cost_b = get(dist_bwd, state_key, typemax(Int))

        # Check if this state lies on *any* path with the exact minimum total cost
        # Avoid potential overflow if costs are typemax(Int) before adding
        if cost_f != typemax(Int) && cost_b != typemax(Int)
            # Use Int128 for the sum check to be absolutely safe against overflow,
            # although Int64 is likely sufficient given typical costs.
             if Int128(cost_f) + Int128(cost_b) == Int128(min_total_cost)
                 # Add the tile coordinates (r, c) to the set of optimal tiles
                 push!(optimal_tiles, (r, c))
             end
        end
    end

    println("Part 2: ", length(optimal_tiles))
end

"""
    main()

Main entry point of the script. Sets up file path, checks for file existence
and readability, calls the `solve` function, and handles potential runtime errors.
"""
function main()
    input_file = "input.txt"

    # Check if input file exists and is a regular file
    if !isfile(input_file)
        println(stderr, "Error: $(input_file) not found or is not a file.")
        exit(1)
    end

    # Optional: Check for read permissions explicitly if needed on some systems
    # try
    #     open(input_file, "r") do f end
    # catch e
    #     println(stderr, "Error: Cannot read $(input_file): ", e)
    #     exit(1)
    # end

    try
        # Execute the main logic
        solve()
    catch e
        # Catch any unexpected errors during execution
        println(stderr, "\nAn unexpected error occurred during execution:")
        # Print the error message and stack trace to standard error for debugging
        showerror(stderr, e, catch_backtrace())
        println(stderr) # Add a newline for clarity
        exit(1) # Exit with a non-zero status code indicating failure
    end
end

# Execute the main function only when the script is run directly
# This standard Julia pattern allows the script to be executed with `julia <script_name>.jl`
# while also allowing functions to be potentially imported and tested elsewhere.
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
