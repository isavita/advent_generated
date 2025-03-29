
using DataStructures

const HALLWAY_ROW = 2
const ROOM_ENTRY_COLS = [4, 6, 8, 10]
const AMPHIPODS = Set(['A', 'B', 'C', 'D'])
const ENERGY_PER_TYPE = Dict('A' => 1, 'B' => 10, 'C' => 100, 'D' => 1000)

struct State
    grid::Matrix{Char}
    energy_used::Int
end

Base.isless(a::State, b::State) = a.energy_used < b.energy_used

function grid_to_string(grid::Matrix{Char})
    return join(String(grid[i, :]) for i in 1:size(grid, 1))
end

function parse_input(filename::String)
    lines = readlines(filename)
    height = length(lines)
    width = length(lines[1])
    grid = Matrix{Char}(undef, height, width)
    for r in 1:height
        for c in 1:width
            if c <= length(lines[r])
                grid[r, c] = lines[r][c]
            else
                grid[r, c] = ' ' # Pad lines if they are uneven, though input format implies they aren't
            end
        end
    end
    return State(grid, 0)
end

function calc_energy(char::Char, start::Tuple{Int, Int}, end_coord::Tuple{Int, Int})
    sr, sc = start
    er, ec = end_coord
    dist = abs(ec - sc) + (sr - HALLWAY_ROW) + (er - HALLWAY_ROW)
    return ENERGY_PER_TYPE[char] * dist
end

function is_room_coord(coord::Tuple{Int, Int}, num_rows::Int)
    r, c = coord
    return r >= HALLWAY_ROW + 1 && r < num_rows && c in ROOM_ENTRY_COLS
end

function get_room_details(coord::Tuple{Int, Int}, room_coord_to_want_char::Dict{Tuple{Int, Int}, Char})
     return get(room_coord_to_want_char, coord, nothing)
end

function all_done(state::State, room_coord_to_want_char::Dict{Tuple{Int, Int}, Char})
    for (coord, want_char) in room_coord_to_want_char
        if state.grid[coord...] != want_char
            return false
        end
    end
    return true
end

function get_unsettled_coords(state::State, room_coord_to_want_char::Dict{Tuple{Int, Int}, Char})
    unsettled = Vector{Tuple{Int, Int}}()
    grid_height, grid_width = size(state.grid)

    # Check hallway
    for c in 2:(grid_width - 1)
        coord = (HALLWAY_ROW, c)
        if state.grid[coord...] in AMPHIPODS
            push!(unsettled, coord)
        end
    end

    # Check rooms
    for col in ROOM_ENTRY_COLS
        room_correct_from_bottom = true
        for row in (grid_height - 1):-1:(HALLWAY_ROW + 1)
             coord = (row, col)
             want_char = get(room_coord_to_want_char, coord, nothing)
             if want_char === nothing continue end # Should not happen with valid inputs

             got_char = state.grid[coord...]

             if got_char == '.'
                 room_correct_from_bottom = false # Deeper spot is empty
                 continue
             end

             if got_char != want_char
                 room_correct_from_bottom = false
                 push!(unsettled, coord)
             elseif !room_correct_from_bottom # got_char == want_char but something below wasn't right
                 push!(unsettled, coord)
             end
        end
    end
    return unsettled
end


function get_next_possible_moves(state::State, unsettled_coord::Tuple{Int, Int}, room_coord_to_want_char::Dict{Tuple{Int, Int}, Char})
    unsettled_char = state.grid[unsettled_coord...]
    possible = Vector{Tuple{Int, Int}}()
    grid_height, grid_width = size(state.grid)

    started_in_hallway = unsettled_coord[1] == HALLWAY_ROW
    target_room_col = 0
    for (coord, wc) in room_coord_to_want_char
        if wc == unsettled_char
             target_room_col = coord[2]
             break
        end
    end

    queue = [(unsettled_coord, 0)] # Store (coord, distance)
    visited = Dict{Tuple{Int, Int}, Int}(unsettled_coord => 0)

    q_idx = 1
    while q_idx <= length(queue)
        front_coord, dist = queue[q_idx]
        q_idx += 1

        # Evaluate 'front_coord' as a potential destination
        if front_coord != unsettled_coord
            fr, fc = front_coord

             # Rule: Cannot stop in hallway outside a room
            is_hallway_stop_point = (fr == HALLWAY_ROW && !(fc in ROOM_ENTRY_COLS))

            # Rule: Can only move into the correct destination room
            is_target_room = (fc == target_room_col && fr > HALLWAY_ROW)

            # Rule: Destination room must be clear (only contain correct amphipods or be empty)
            target_room_clear = true
            if is_target_room
                for r in (fr + 1):(grid_height - 1)
                    if state.grid[r, fc] != '.' && state.grid[r, fc] != unsettled_char
                         target_room_clear = false
                         break
                    end
                end
                 # Also need to ensure we move to the deepest available spot
                 if target_room_clear
                     for r in (fr + 1):(grid_height - 1)
                         if state.grid[r, fc] == '.'
                             target_room_clear = false # Cannot stop here if deeper is empty
                             break
                         end
                     end
                 end
            end

            if started_in_hallway
                # Must move into a room, specifically the correct, clear room
                if is_target_room && target_room_clear
                    push!(possible, front_coord)
                     # Once a path to the room is found, no need to explore further from hallway
                     # But BFS explores level by level, so we just collect valid room destinations
                end
            else # Started in a room
                # Can move to hallway stop points OR the correct, clear room
                 if is_hallway_stop_point
                     push!(possible, front_coord)
                 elseif is_target_room && target_room_clear
                     push!(possible, front_coord)
                     # If we find the target room, this is the only valid type of move out of the initial room state
                     # However, BFS finds shortest path, so other hallway moves might be necessary first steps
                 end
            end
        end

        # Explore neighbors for BFS
        for (dr, dc) in [(-1, 0), (1, 0), (0, -1), (0, 1)]
            next_row, next_col = front_coord[1] + dr, front_coord[2] + dc

            if 1 <= next_row <= grid_height && 1 <= next_col <= grid_width && state.grid[next_row, next_col] == '.'
                next_coord = (next_row, next_col)
                if !haskey(visited, next_coord) || visited[next_coord] > dist + 1
                    visited[next_coord] = dist + 1
                    push!(queue, (next_coord, dist + 1))
                end
            end
        end
    end

    # Filter: If moving from Hallway, only allow moves into the target room.
    # If moving from Room, allow hallway moves OR target room moves.
    # If any target room move is possible, prune hallway moves if moving from a Room? No, the path might go via hallway.
    # Let Dijkstra handle the cost. The BFS just finds reachable empty squares adhering to basic rules.

    # The Python logic is slightly different: it checks target room viability *during* BFS.
    # Let's refine the possible destinations based on the Python logic's final checks.

    final_possible = Vector{Tuple{Int, Int}}()
    can_reach_target_room = false
    target_room_destinations = Vector{Tuple{Int, Int}}()

    for p_coord in possible
        pr, pc = p_coord
        is_target_room = (pc == target_room_col && pr > HALLWAY_ROW)
        if is_target_room
            push!(target_room_destinations, p_coord)
            can_reach_target_room = true
        end
    end

    if started_in_hallway
        # If started in hallway, *must* go to target room if possible.
        # The BFS already ensured target room validity (clear, deepest).
        final_possible = target_room_destinations # Only target room moves are valid
    else # Started in room
        if can_reach_target_room
             # If we can reach the target room directly, that's the preferred final destination type.
             # Add *only* the best target room destination found by BFS.
             # (Python logic implies only one such best move is considered per unsettled amphipod)
             # Let's add all valid destinations found by BFS. Dijkstra will pick the cheapest path.
             final_possible = possible # Allow hallway stops and target room moves
        else
             # Cannot reach target room, only hallway moves are possible.
             for p_coord in possible
                 pr, pc = p_coord
                 is_hallway_stop_point = (pr == HALLWAY_ROW && !(pc in ROOM_ENTRY_COLS))
                 if is_hallway_stop_point
                     push!(final_possible, p_coord)
                 end
             end
        end
    end


    # Check Python logic again:
    # BFS finds *all* reachable '.' spots.
    # Then it filters:
    #   If spot is hallway (not above room): Add if not started_in_hallway.
    #   If spot is room: Check if want_char == unsettled_char. Check if deeper spots are clear/correct. Add if valid.
    # This implies a simpler filtering post-BFS. Let's reimplement based on that.

    possible = Vector{Tuple{Int, Int}}() # Reset and rebuild based on Python logic
    queue = [(unsettled_coord, 0)]
    visited = Dict{Tuple{Int, Int}, Int}(unsettled_coord => 0)
    q_idx = 1

    while q_idx <= length(queue)
        front_coord, dist = queue[q_idx]
        q_idx += 1

        if front_coord != unsettled_coord
             fr, fc = front_coord
             is_hallway_spot = fr == HALLWAY_ROW
             is_above_room = is_hallway_spot && fc in ROOM_ENTRY_COLS

             want_char = get(room_coord_to_want_char, front_coord, nothing)

             if is_hallway_spot && !is_above_room
                 # Valid hallway destination if started in a room
                 if !started_in_hallway
                     push!(possible, front_coord)
                 end
             elseif want_char !== nothing && want_char == unsettled_char
                 # Potential target room destination
                 room_clear_below = true
                 can_stop_here = true
                 for r in (fr + 1):(grid_height - 1)
                     char_below = state.grid[r, fc]
                     if char_below == '.'
                         can_stop_here = false # Must move to deepest spot
                         break
                     elseif char_below != want_char
                         room_clear_below = false # Blocked by wrong amphipod
                         break
                     end
                 end

                 if room_clear_below && can_stop_here
                     push!(possible, front_coord)
                 end
             end
        end

         # Explore neighbors
         for (dr, dc) in [(-1, 0), (1, 0), (0, -1), (0, 1)]
            next_row, next_col = front_coord[1] + dr, front_coord[2] + dc
            if 1 <= next_row <= grid_height && 1 <= next_col <= grid_width && state.grid[next_row, next_col] == '.'
                next_coord = (next_row, next_col)
                 # Use simple Set for visited in BFS pathfinding
                if !haskey(visited, next_coord)
                     visited[next_coord] = 1 # Mark as visited
                     push!(queue, (next_coord, dist + 1)) # Distance not used for destination check here
                 end
            end
         end
    end

    # If starting in hallway, filter to *only* include room destinations
    if started_in_hallway
        filter!(p -> p[1] > HALLWAY_ROW, possible)
    end

    # Optimization: If multiple room destinations are possible, only keep the deepest one.
    deepest_room_moves = Dict{Int, Tuple{Int, Int}}() # col -> deepest_coord
    final_possible = Vector{Tuple{Int, Int}}()
    for p_coord in possible
        pr, pc = p_coord
        if pr > HALLWAY_ROW # It's a room move
            if !haskey(deepest_room_moves, pc) || pr > deepest_room_moves[pc][1]
                deepest_room_moves[pc] = p_coord
            end
        else # It's a hallway move
            push!(final_possible, p_coord)
        end
    end
    for room_move in values(deepest_room_moves)
        push!(final_possible, room_move)
    end

    # If we started in a room and found a valid room move, we *must* take it (no hallway stops allowed then).
    # Check if any final moves are room moves
    has_room_move = any(p -> p[1] > HALLWAY_ROW, final_possible)
    if !started_in_hallway && has_room_move
        filter!(p -> p[1] > HALLWAY_ROW, final_possible)
    end


    return final_possible

end


function solve()
    filename = "input.txt"
    start_state = parse_input(filename)
    grid_height = size(start_state.grid, 1)

    # Adjust room coords based on actual grid height (for Part 1 vs Part 2)
    room_coord_to_want_char = Dict{Tuple{Int, Int}, Char}()
    room_depth = grid_height - 1 - HALLWAY_ROW
    room_chars = ['A', 'B', 'C', 'D']
    for i in 1:4
        char = room_chars[i]
        col = ROOM_ENTRY_COLS[i]
        for depth in 1:room_depth
            row = HALLWAY_ROW + depth
            room_coord_to_want_char[(row, col)] = char
        end
    end


    min_heap = PriorityQueue{State, Int}()
    enqueue!(min_heap, start_state => start_state.energy_used)
    seen_grids = Set{String}()

    while !isempty(min_heap)
        current_state = dequeue!(min_heap)

        grid_key = grid_to_string(current_state.grid)
        if grid_key in seen_grids
            continue
        end
        push!(seen_grids, grid_key)

        if all_done(current_state, room_coord_to_want_char)
            return current_state.energy_used
        end

        unsettled = get_unsettled_coords(current_state, room_coord_to_want_char)

        for unsettled_coord in unsettled
            moves = get_next_possible_moves(current_state, unsettled_coord, room_coord_to_want_char)

            for next_coord in moves
                ur, uc = unsettled_coord
                nr, nc = next_coord
                char_to_move = current_state.grid[ur, uc]

                energy_cost = calc_energy(char_to_move, unsettled_coord, next_coord)

                new_grid = copy(current_state.grid)
                new_grid[nr, nc] = char_to_move
                new_grid[ur, uc] = '.'

                new_state = State(new_grid, current_state.energy_used + energy_cost)

                new_grid_key = grid_to_string(new_grid)
                if !(new_grid_key in seen_grids) # Check before enqueuing
                     enqueue!(min_heap, new_state => new_state.energy_used)
                end

            end
        end
    end

    error("Should have found a solution")
end

function main()
    result = solve()
    println(result)
end

main()
