
using Printf

# --- Constants ---
# Directions: 0:Right (>), 1:Down (v), 2:Left (<), 3:Up (^)
const DR = [0, 1, 0, -1]
const DC = [1, 0, -1, 0]
const FACE_SIZE = 50 # For Part 2 (adjust if using example: 4)

# --- Input Parsing ---
function parse_input(filename::String)
    lines = readlines(filename)
    map_lines = String[]
    path_str = ""
    max_width = 0

    # Separate map and path
    for line in lines
        if isempty(line)
            continue # Skip empty lines between map and path
        elseif occursin(r"^\s*[\.#]", line) # Map line
            push!(map_lines, line)
            max_width = max(max_width, length(line))
        else # Path line
            path_str = line
        end
    end

    # Pad map lines to create a rectangular grid
    padded_map_lines = [rpad(line, max_width) for line in map_lines]
    grid = permutedims(hcat(map(collect, padded_map_lines)...)) # Creates Matrix{Char}

    # Parse path string (e.g., "10R5L5") into instructions
    instructions = []
    current_num = ""
    for char in path_str
        if isdigit(char)
            current_num *= char
        else
            if !isempty(current_num)
                push!(instructions, parse(Int, current_num))
                current_num = ""
            end
            push!(instructions, char)
        end
    end
    if !isempty(current_num) # Add last number if path ends with digits
        push!(instructions, parse(Int, current_num))
    end

    return grid, instructions
end

# --- Part 1: Flat Map Movement ---

# Find the next valid position, handling wrapping
function find_next_pos_part1(r::Int, c::Int, facing::Int, grid::Matrix{Char})
    rows, cols = size(grid)
    dr, dc = DR[facing+1], DC[facing+1]
    nr, nc = r + dr, c + dc

    # Check for wrap-around
    if nr < 1 || nr > rows || nc < 1 || nc > cols || grid[nr, nc] == ' '
        if facing == 0 # Right -> Wrap Left
            nc = findfirst(x -> x != ' ', grid[r, :])
            nr = r
        elseif facing == 1 # Down -> Wrap Up
            nr = findfirst(x -> x != ' ', grid[:, c])
            nc = c
        elseif facing == 2 # Left -> Wrap Right
            nc = findlast(x -> x != ' ', grid[r, :])
            nr = r
        elseif facing == 3 # Up -> Wrap Down
            nr = findlast(x -> x != ' ', grid[:, c])
            nc = c
        end
    end
    return nr, nc
end

function solve_part1(grid::Matrix{Char}, instructions::Vector)
    rows, cols = size(grid)

    # Find starting position (leftmost '.' in top row)
    r = 1
    c = findfirst(x -> x == '.', grid[1, :])
    facing = 0 # 0:Right

    # Simulate movement
    for instruction in instructions
        if isa(instruction, Int) # Move
            steps = instruction
            for _ in 1:steps
                next_r, next_c = find_next_pos_part1(r, c, facing, grid)
                if grid[next_r, next_c] == '#'
                    break # Hit a wall, stop moving for this instruction
                else
                    r, c = next_r, next_c # Move to the open tile
                end
            end
        else # Turn
            turn = instruction
            if turn == 'R'
                facing = (facing + 1) % 4
            elseif turn == 'L'
                facing = (facing + 3) % 4
            end
        end
    end

    # Calculate final password
    return 1000 * r + 4 * c + facing
end

# --- Part 2: Cube Map Movement ---

# Determine face ID and local coordinates (1-based)
# Hardcoded layout for the standard 50x50 AoC input:
#   1122
#   1122
#   33
#   33
# 4455
# 4455
# 66
# 66
function get_face_info(r::Int, c::Int, side_len::Int)
    # Face 1
    if 1 <= r <= side_len && side_len + 1 <= c <= 2 * side_len
        return 1, r, c - side_len
    # Face 2
    elseif 1 <= r <= side_len && 2 * side_len + 1 <= c <= 3 * side_len
         return 2, r, c - 2 * side_len
    # Face 3
    elseif side_len + 1 <= r <= 2 * side_len && side_len + 1 <= c <= 2 * side_len
        return 3, r - side_len, c - side_len
    # Face 4
    elseif 2 * side_len + 1 <= r <= 3 * side_len && 1 <= c <= side_len
         return 4, r - 2 * side_len, c
    # Face 5
    elseif 2 * side_len + 1 <= r <= 3 * side_len && side_len + 1 <= c <= 2 * side_len
        return 5, r - 2 * side_len, c - side_len
    # Face 6
    elseif 3 * side_len + 1 <= r <= 4 * side_len && 1 <= c <= side_len
         return 6, r - 3 * side_len, c
    else
        error("Invalid coordinates ($r, $c) for face mapping")
    end
end

# Determine global coordinates from face ID and local coordinates (1-based)
function get_global_coords(face_id::Int, fr::Int, fc::Int, side_len::Int)
    if face_id == 1
        return fr, fc + side_len
    elseif face_id == 2
         return fr, fc + 2 * side_len
    elseif face_id == 3
        return fr + side_len, fc + side_len
    elseif face_id == 4
         return fr + 2 * side_len, fc
    elseif face_id == 5
        return fr + 2 * side_len, fc + side_len
    elseif face_id == 6
         return fr + 3 * side_len, fc
    else
        error("Invalid face_id $face_id")
    end
end

# Find next position and facing, handling cube edge transitions
# This function implements the hardcoded cube net wrapping logic.
function find_next_pos_part2(r::Int, c::Int, facing::Int, grid::Matrix{Char}, side_len::Int)
    rows, cols = size(grid)
    dr, dc = DR[facing+1], DC[facing+1]
    nr, nc = r + dr, c + dc
    nfacing = facing # Facing usually stays the same unless wrapping

    # Check if the potential next step is off-grid or onto a space ' '
    # This indicates a potential face transition
    if nr < 1 || nr > rows || nc < 1 || nc > cols || grid[nr, nc] == ' '
        face_id, fr, fc = get_face_info(r, c, side_len)
        # Determine the target face, coordinates, and facing based on current face and exit direction
        # (fr, fc) are 1-based local coordinates on the current face
        # Target coordinates (nfr, nfc) and facing (nfacing)
        nfr, nfc = -1, -1 # Initialize invalid
        target_face_id = -1

        if face_id == 1
            if facing == 2 # Left edge of Face 1
                target_face_id = 4
                nfr = side_len - fr + 1 # Reverse row
                nfc = 1                 # Enter from left edge
                nfacing = 0             # New facing: Right
            elseif facing == 3 # Top edge of Face 1
                target_face_id = 6
                nfr = fc                # Row becomes original col
                nfc = 1                 # Enter from left edge
                nfacing = 0             # New facing: Right
            end
        elseif face_id == 2
            if facing == 0 # Right edge of Face 2
                target_face_id = 5
                nfr = side_len - fr + 1 # Reverse row
                nfc = side_len          # Enter from right edge
                nfacing = 2             # New facing: Left
            elseif facing == 1 # Bottom edge of Face 2
                target_face_id = 3
                nfr = fc                # Row becomes original col
                nfc = side_len          # Enter from right edge
                nfacing = 2             # New facing: Left
            elseif facing == 3 # Top edge of Face 2
                target_face_id = 6
                nfr = side_len          # Enter from bottom edge
                nfc = fc                # Col remains the same
                nfacing = 3             # New facing: Up (no change)
            end
        elseif face_id == 3
             if facing == 0 # Right edge of Face 3
                target_face_id = 2
                nfr = side_len          # Enter from bottom edge
                nfc = fr                # Col becomes original row
                nfacing = 3             # New facing: Up
             elseif facing == 2 # Left edge of Face 3
                target_face_id = 4
                nfr = 1                 # Enter from top edge
                nfc = fr                # Col becomes original row
                nfacing = 1             # New facing: Down
            end
        elseif face_id == 4
             if facing == 2 # Left edge of Face 4
                target_face_id = 1
                nfr = side_len - fr + 1 # Reverse row
                nfc = 1                 # Enter from left edge
                nfacing = 0             # New facing: Right
             elseif facing == 3 # Top edge of Face 4
                target_face_id = 3
                nfr = fc                # Row becomes original col
                nfc = 1                 # Enter from left edge
                nfacing = 0             # New facing: Right
            end
        elseif face_id == 5
            if facing == 0 # Right edge of Face 5
                target_face_id = 2
                nfr = side_len - fr + 1 # Reverse row
                nfc = side_len          # Enter from right edge
                nfacing = 2             # New facing: Left
            elseif facing == 1 # Bottom edge of Face 5
                target_face_id = 6
                nfr = fc                # Row becomes original col
                nfc = side_len          # Enter from right edge
                nfacing = 2             # New facing: Left
            end
        elseif face_id == 6
            if facing == 0 # Right edge of Face 6
                target_face_id = 5
                nfr = side_len          # Enter from bottom edge
                nfc = fr                # Col becomes original row
                nfacing = 3             # New facing: Up
            elseif facing == 1 # Bottom edge of Face 6
                target_face_id = 2
                nfr = 1                 # Enter from top edge
                nfc = fc                # Col remains the same
                nfacing = 1             # New facing: Down (no change)
            elseif facing == 2 # Left edge of Face 6
                target_face_id = 1
                nfr = 1                 # Enter from top edge
                nfc = fr                # Col becomes original row
                nfacing = 1             # New facing: Down
            end
        end

        # Convert back to global coordinates
        nr, nc = get_global_coords(target_face_id, nfr, nfc, side_len)

    end # End wrap handling

    return nr, nc, nfacing
end


function solve_part2(grid::Matrix{Char}, instructions::Vector, side_len::Int)
    rows, cols = size(grid)

    # Find starting position (same as part 1)
    r = 1
    c = findfirst(x -> x == '.', grid[1, :])
    facing = 0 # 0:Right

    # Simulate movement
    for instruction in instructions
        if isa(instruction, Int) # Move
            steps = instruction
            for _ in 1:steps
                next_r, next_c, next_facing = find_next_pos_part2(r, c, facing, grid, side_len)
                # Check the tile at the potential next position
                if grid[next_r, next_c] == '#'
                    break # Hit a wall, stop moving for this instruction
                else
                    # Move successful, update state
                    r, c, facing = next_r, next_c, next_facing
                end
            end
        else # Turn
            turn = instruction
            if turn == 'R'
                facing = (facing + 1) % 4
            elseif turn == 'L'
                facing = (facing + 3) % 4
            end
        end
    end

    # Calculate final password
    return 1000 * r + 4 * c + facing
end


# --- Main Execution ---
function main()
    if length(ARGS) == 0
         input_file = "input.txt"
         println("Using default input file: $input_file")
     else
         input_file = ARGS[1]
         println("Using input file: $input_file")
     end

    if !isfile(input_file)
        println("Error: Input file '$input_file' not found.")
        # Create a dummy input.txt for testing if needed
        println("Creating dummy 'input.txt'. Please replace with actual puzzle input.")
        open(input_file, "w") do f
            write(f, """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
""")
        end
        # return # Exit if you don't want to run with dummy data
    end


    grid, instructions = parse_input(input_file)

    # Determine side length heuristically (assuming square faces)
    total_area = sum(c != ' ' for c in grid)
    side_len = isqrt(div(total_area, 6)) # Integer square root
     if side_len * side_len * 6 != total_area
         @warn "Heuristic side length calculation might be inaccurate. Total area ($total_area) not divisible by 6 into a perfect square. Assuming $FACE_SIZE."
         side_len = FACE_SIZE # Fallback to constant
     else
        println("Detected cube face side length: $side_len")
    end


    # Solve Part 1
    part1_result = solve_part1(grid, instructions)
    @printf("Part 1: %d\n", part1_result)

    # Solve Part 2
    part2_result = solve_part2(grid, instructions, side_len)
    @printf("Part 2: %d\n", part2_result)

end

# Run the main function
main()
