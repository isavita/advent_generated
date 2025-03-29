
using Printf

# Checks if coordinates are within the grid boundaries
function is_valid(r::Int, c::Int, rows::Int, cols::Int)::Bool
    return 1 <= r <= rows && 1 <= c <= cols
end

# Recursive function to push boxes. Modifies the grid in place.
function push_boxes!(grid::Matrix{Char}, r::Int, c::Int, dr::Int, dc::Int, rows::Int, cols::Int)::Bool
    nr, nc = r + dr, c + dc

    # Check bounds first
    if !is_valid(nr, nc, rows, cols)
        return false
    end

    # Check for wall
    if grid[nr, nc] == '#'
        return false
    end

    # Check for another box - requires recursive push
    if grid[nr, nc] == 'O'
        # Try pushing the next box. If it fails, we fail.
        if !push_boxes!(grid, nr, nc, dr, dc, rows, cols)
            return false
        end
        # If the recursive push succeeded, the space grid[nr, nc] should now be '.'
        # We fall through to the next step.
    end

    # At this point, grid[nr, nc] should be '.' (either initially or after a successful recursive push)
    if grid[nr, nc] == '.'
         grid[nr, nc] = 'O' # Place the current box
         grid[r, c] = '.'   # Clear the current box's old spot
         return true
    else
         # Cannot push into this spot (e.g., '@' or unexpected character)
         return false
    end
end

function main()
    lines = readlines("input.txt")

    grid_list = Vector{Vector{Char}}()
    moves_buffer = IOBuffer()
    reading_map = true
    max_cols = 0

    # Parse map and moves
    for line in lines
        s_line = strip(line) # Remove leading/trailing whitespace for checks
        if reading_map
            if isempty(s_line) # Empty line separates map from moves
                 if !isempty(grid_list) # Ensure we saw map lines before the separator
                    reading_map = false
                 end
                 continue # Skip processing the empty line
            elseif occursin(r"[#@O\.]", line) # Check if line looks like part of the map
                 row_chars = collect(line)
                 push!(grid_list, row_chars)
                 max_cols = max(max_cols, length(row_chars))
            elseif !isempty(grid_list) # If not empty, not map-like, and we already have map lines, it's moves
                 reading_map = false
                 print(moves_buffer, line) # Start collecting moves
            else
                 # Ignore lines before the first map line if they don't match map chars
                 continue
            end
        else
            print(moves_buffer, line) # Continue collecting moves
        end
    end
    moves = String(take!(moves_buffer))

    rows = length(grid_list)
    if rows == 0 || max_cols == 0
        println(0) # Handle empty or invalid map input
        return
    end

    # Create and pad the grid Matrix. Padding with ' ' (space).
    # Assuming spaces are treated as impassable areas or outside the playable zone.
    grid = fill(' ', rows, max_cols)
    robot_r, robot_c = -1, -1 # Use 1-based indexing

    for r in 1:rows
        row_len = length(grid_list[r])
        for c in 1:row_len
            char = grid_list[r][c]
            grid[r, c] = char
            if char == '@'
                robot_r, robot_c = r, c
            end
        end
    end

    if robot_r == -1
         println("Error: Robot '@' not found in grid.")
         return
    end

    # Directions mapping (row, col change)
    dirs = Dict{Char, Tuple{Int, Int}}(
        '^' => (-1, 0),
        'v' => (1, 0),
        '<' => (0, -1),
        '>' => (0, 1)
    )

    # Process moves
    for move in moves
        if !haskey(dirs, move) continue end # Skip invalid move characters

        dr, dc = dirs[move]
        nr, nc = robot_r + dr, robot_c + dc

        # Bounds check for the robot's potential next move
        if !is_valid(nr, nc, rows, max_cols) continue end

        target_char = grid[nr, nc]

        # Handle wall
        if target_char == '#'
            continue
        # Handle box
        elseif target_char == 'O'
            # Try pushing the box. If push fails, do nothing.
            if !push_boxes!(grid, nr, nc, dr, dc, rows, max_cols)
                continue
            end
            # If push succeeded, grid[nr, nc] is now '.', fall through to move the robot
            # Update target_char state for the next check
            target_char = '.' # The space is now clear
        end

         # Handle empty space '.' or a space just cleared by a box push ('O' became '.')
        if target_char == '.'
            grid[robot_r, robot_c] = '.' # Clear robot's old spot
            grid[nr, nc] = '@'           # Move robot
            robot_r, robot_c = nr, nc    # Update robot position
        end
        # Implicit else: if target_char is something else (' ' padding, etc.), do nothing.
    end

    # Calculate final sum using 0-based indexing for score as per Python code
    total_sum = 0
    for r in 1:rows
        for c in 1:max_cols
            if grid[r, c] == 'O'
                total_sum += (r - 1) * 100 + (c - 1)
            end
        end
    end

    println(total_sum)
end

main()
