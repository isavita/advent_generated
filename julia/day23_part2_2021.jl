
using DataStructures

const ENERGY_PER_TYPE = Dict('A' => 1, 'B' => 10, 'C' => 100, 'D' => 1000)

mutable struct State
    grid::Vector{Vector{Char}}
    energy_used::Int
end

Base.isless(a::State, b::State) = a.energy_used < b.energy_used

function deepcopy_state(s::State)
    return State(deepcopy(s.grid), s.energy_used)
end

function grid_to_tuple(grid::Vector{Vector{Char}})
    return Tuple(Tuple(row) for row in grid)
end

# Adjust indices for 1-based Julia and inserted rows
const ROOM_COORD_TO_WANT_CHAR = Dict{Tuple{Int, Int}, Char}(
    (3, 4) => 'A', (4, 4) => 'A', (5, 4) => 'A', (6, 4) => 'A', # Room A (col 3+1)
    (3, 6) => 'B', (4, 6) => 'B', (5, 6) => 'B', (6, 6) => 'B', # Room B (col 5+1)
    (3, 8) => 'C', (4, 8) => 'C', (5, 8) => 'C', (6, 8) => 'C', # Room C (col 7+1)
    (3, 10) => 'D', (4, 10) => 'D', (5, 10) => 'D', (6, 10) => 'D' # Room D (col 9+1)
)

const ROOM_COLS = Dict('A' => 4, 'B' => 6, 'C' => 8, 'D' => 10)
const HALLWAY_ROW = 2 # Julia index for row 1 in Python
const MAX_ROOM_DEPTH = 6 # Julia index for deepest row

function is_all_done(state::State)
    for (coord, want) in ROOM_COORD_TO_WANT_CHAR
        if state.grid[coord[1]][coord[2]] != want
            return false
        end
    end
    return true
end

function get_unsettled_coords(state::State)
    unsettled = Tuple{Int, Int}[]
    grid = state.grid
    rows = length(grid)
    cols = length(grid[1])

    # Check hallway
    for c in 2:(cols-1) # Skip walls
        if grid[HALLWAY_ROW][c] in "ABCD"
            push!(unsettled, (HALLWAY_ROW, c))
        end
    end

    # Check rooms
    for col in values(ROOM_COLS)
        room_full_from_back = true
        for row in MAX_ROOM_DEPTH:-1:3 # Check from bottom up (Julia index 3 is first room row)
             coord = (row, col)
             want_char = get(ROOM_COORD_TO_WANT_CHAR, coord, '?') # Should always exist
             got_char = grid[row][col]

             if got_char != '.'
                 if got_char != want_char
                     room_full_from_back = false
                     push!(unsettled, coord)
                 elseif got_char == want_char && !room_full_from_back
                     # Correct type, but blocking a wrong one deeper in
                     push!(unsettled, coord)
                 end
             else # Empty spot means deeper ones are not blocked by this level
                 room_full_from_back = false
             end
        end
    end
    return unsettled
end

function get_next_possible_moves(state::State, unsettled_coord::Tuple{Int, Int})
    grid = state.grid
    rows = length(grid)
    cols = length(grid[1])
    unsettled_char = grid[unsettled_coord[1]][unsettled_coord[2]]
    if !(unsettled_char in "ABCD")
        error("Unexpected character to get next moves for: $unsettled_char")
    end

    possible = Tuple{Int, Int}[]
    started_in_hallway = unsettled_coord[1] == HALLWAY_ROW
    target_col = ROOM_COLS[unsettled_char]

    queue = Tuple{Int, Int}[(unsettled_coord)]
    visited = Set{Tuple{Int, Int}}()
    dist = Dict{Tuple{Int, Int}, Int}(unsettled_coord => 0)

    while !isempty(queue)
        front = popfirst!(queue)
        if front in visited
            continue
        end
        push!(visited, front)
        current_dist = dist[front]

        # Check if this is a valid destination
        if front != unsettled_coord
             # Rule: Cannot stop in hallway space directly outside any room
            if !(front[1] == HALLWAY_ROW && front[2] in values(ROOM_COLS))
                # Moving from Hallway to Room:
                if started_in_hallway
                    # Must be the correct room column
                    if front[2] == target_col && front[1] > HALLWAY_ROW
                        # Check if room is valid (only contains correct type or is empty deeper)
                        can_enter_room = true
                        deepest_available = -1
                        for r in 3:MAX_ROOM_DEPTH
                            room_char = grid[r][front[2]]
                            if room_char == '.'
                                deepest_available = r # Track deepest empty
                            elseif room_char != unsettled_char
                                can_enter_room = false # Found wrong type
                                break
                            end
                        end
                        # Must move to the deepest available spot
                        if can_enter_room && front[1] == deepest_available
                             push!(possible, front)
                        end
                    end
                # Moving from Room to Hallway:
                else
                    # Can stop in any valid hallway spot
                    if front[1] == HALLWAY_ROW
                        push!(possible, front)
                    end
                     # Implicitly handles moving Room to Room via Hallway by generating Hallway stops first
                end
            end
        end

        # Explore neighbors (BFS for reachability)
        for (dr, dc) in [(-1, 0), (1, 0), (0, -1), (0, 1)]
            next_row, next_col = front[1] + dr, front[2] + dc
            # Check bounds and if it's a walkable space '.'
            if 1 <= next_row <= rows && 1 <= next_col <= cols && grid[next_row][next_col] == '.'
                neighbor = (next_row, next_col)
                if !(neighbor in visited) # Check visited before adding to queue
                   dist[neighbor] = current_dist + 1
                   push!(queue, neighbor)
                end
            end
        end
    end

    return possible, dist
end

function calc_energy(char::Char, start::Tuple{Int, Int}, end_pos::Tuple{Int, Int}, path_dist::Int)
    energy_cost = get(ENERGY_PER_TYPE, char, 0)
    if energy_cost == 0
         error("Unexpected character: $char")
    end
    # Manhattan distance adjusted by BFS path distance is more accurate
    # dist = abs(end_pos[2] - start[2]) + abs(end_pos[1] - start[1]) # Incorrect for burrow paths
    return energy_cost * path_dist
end


function amphipod(input_str::String)
    grid_lines = split(strip(input_str), '\n')
    grid = [collect(line) for line in grid_lines]

    # --- Part 2 Modification ---
    # Insert the two extra lines for Part 2
    # Indices shift: original row 3 becomes 4, original row 4 becomes 6
    insert!(grid, 4, collect("  #D#C#B#A#"))
    insert!(grid, 5, collect("  #D#B#A#C#"))
    # Grid now has 7 rows (1-based index)

    start_state = State(grid, 0)

    # Use BinaryMinHeap based on energy cost
    min_heap = BinaryMinHeap{State}()
    push!(min_heap, start_state)

    # Use Tuple representation of grid for efficient hashing in seen set
    seen_grids = Dict{Tuple, Int}()
    seen_grids[grid_to_tuple(start_state.grid)] = 0

    min_energy = typemax(Int)


    while !isempty(min_heap)
        current_state = pop!(min_heap)

        current_grid_tuple = grid_to_tuple(current_state.grid)

        # If we found a shorter path to this state already, skip
        if get(seen_grids, current_grid_tuple, typemax(Int)) < current_state.energy_used
            continue
        end

        if is_all_done(current_state)
            min_energy = min(min_energy, current_state.energy_used)
            # Don't return early, Dijkstra needs to explore fully if costs differ
             continue # Found a potential solution, but maybe not the best yet
        end

        # Pruning: If current energy exceeds best found so far, prune branch
        if current_state.energy_used >= min_energy
            continue
        end

        unsettled_coords = get_unsettled_coords(current_state)

        for unsettled_coord in unsettled_coords
            unsettled_char = current_state.grid[unsettled_coord[1]][unsettled_coord[2]]
            possible_moves, distances = get_next_possible_moves(current_state, unsettled_coord)

            for next_coord in possible_moves
                path_dist = distances[next_coord] # Get distance from BFS
                energy_cost = calc_energy(unsettled_char, unsettled_coord, next_coord, path_dist)
                new_energy = current_state.energy_used + energy_cost

                # Create the next state by copying and modifying
                next_grid = deepcopy(current_state.grid)
                next_grid[next_coord[1]][next_coord[2]] = unsettled_char
                next_grid[unsettled_coord[1]][unsettled_coord[2]] = '.'

                next_grid_tuple = grid_to_tuple(next_grid)

                # If this new state is better than any previously seen version
                if new_energy < get(seen_grids, next_grid_tuple, typemax(Int))
                    next_state = State(next_grid, new_energy)
                    push!(min_heap, next_state)
                    seen_grids[next_grid_tuple] = new_energy
                end
            end
        end
    end

    if min_energy == typemax(Int)
        error("Solution not found")
    end
    return min_energy
end

function main()
    try
        input_str = read("input.txt", String)
        result = amphipod(input_str)
        println(result)
    catch e
        println("Error during execution: ", e)
        if isa(e, SystemError) && e.errval == 2 # ENOENT: No such file or directory
             println("Ensure 'input.txt' exists in the same directory.")
        end
         # Optional: Print stacktrace for debugging
         #println(stacktrace(catch_backtrace()))
    end
end

main()
