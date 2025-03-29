
using DataStructures

function read_input(file_path::String)::Matrix{Int}
    lines = readlines(file_path)
    height = length(lines)
    width = length(strip(lines[1]))
    grid = Matrix{Int}(undef, height, width)
    for (i, line) in enumerate(lines)
        grid[i, :] = parse.(Int, collect(strip(line)))
    end
    return grid
end

const DIRECTIONS = Dict{Char, Tuple{Int, Int}}(
    'N' => (-1, 0), # Row change, Col change
    'S' => (1, 0),
    'E' => (0, 1),
    'W' => (0, -1)
)

const DIRECTION_TURNS = Dict{Char, Dict{Char, Char}}(
    'N' => Dict('L' => 'W', 'R' => 'E'),
    'S' => Dict('L' => 'E', 'R' => 'W'),
    'E' => Dict('L' => 'N', 'R' => 'S'),
    'W' => Dict('L' => 'S', 'R' => 'N')
)

function solve(grid::Matrix{Int}, part::Int)::Int
    height, width = size(grid)
    end_pos = (height, width)

    min_steps_before_turn = (part == 1) ? 1 : 4
    max_steps = (part == 1) ? 3 : 10

    pq = BinaryMinHeap{Tuple{Int, Int, Int, Char, Int}}()
    visited = Dict{Tuple{Int, Int, Char, Int}, Int}()

    # Initial moves from (1, 1)
    # State: (total_heat_loss, row, col, direction, steps_in_current_direction)
    for start_dir in ['E', 'S']
         dr, dc = DIRECTIONS[start_dir]
         nr, nc = 1 + dr, 1 + dc
         if 1 <= nr <= height && 1 <= nc <= width
            start_heat = grid[nr, nc]
            start_state = (start_heat, nr, nc, start_dir, 1)
            push!(pq, start_state)
            # No need to add to visited here, will be handled when popped
         end
    end


    while !isempty(pq)
        total_heat, r, c, dir, steps = pop!(pq)

        state_key = (r, c, dir, steps)
        if haskey(visited, state_key) && visited[state_key] <= total_heat
            continue
        end
        visited[state_key] = total_heat

        if (r, c) == end_pos && (part == 1 || steps >= min_steps_before_turn)
            return total_heat
        end


        # 1. Continue Straight
        if steps < max_steps
            dr, dc = DIRECTIONS[dir]
            nr, nc = r + dr, c + dc
            if 1 <= nr <= height && 1 <= nc <= width
                new_heat = total_heat + grid[nr, nc]
                new_steps = steps + 1
                new_state_key = (nr, nc, dir, new_steps)
                if !haskey(visited, new_state_key) || new_heat < visited[new_state_key]
                     push!(pq, (new_heat, nr, nc, dir, new_steps))
                end
            end
        end

        # 2. Turn Left/Right
        if steps >= min_steps_before_turn
            for turn_char in ['L', 'R']
                new_dir = DIRECTION_TURNS[dir][turn_char]
                dr, dc = DIRECTIONS[new_dir]
                nr, nc = r + dr, c + dc
                if 1 <= nr <= height && 1 <= nc <= width
                    new_heat = total_heat + grid[nr, nc]
                    new_steps = 1
                    new_state_key = (nr, nc, new_dir, new_steps)
                     if !haskey(visited, new_state_key) || new_heat < visited[new_state_key]
                         push!(pq, (new_heat, nr, nc, new_dir, new_steps))
                     end
                end
            end
        end
    end

    return -1
end


function main()
    grid = read_input("input.txt")

    result_part1 = solve(grid, 1)
    println(result_part1)

    result_part2 = solve(grid, 2)
    println(result_part2)
end

main()
