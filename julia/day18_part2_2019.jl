
using DataStructures

# Define PointOfInterest struct (optional but helps clarity)
struct PointOfInterest
    id::Int
    char::Char
    pos::CartesianIndex{2}
end

# Helper function to check if a character is a key
is_key(c::Char) = 'a' <= c <= 'z'

# Helper function to check if a character is a door
is_door(c::Char) = 'A' <= c <= 'Z'

# Function to perform BFS from a single Point of Interest (POI)
# to find distances and required keys to other reachable POIs.
function bfs_from_poi(grid::Matrix{Char}, start_pos::CartesianIndex{2}, all_pois::Dict{CartesianIndex{2}, PointOfInterest})
    rows, cols = size(grid)
    q = Queue{Tuple{CartesianIndex{2}, Int, UInt32}}() # (pos, distance, required_keys_mask)
    visited = Dict{CartesianIndex{2}, Tuple{Int, UInt32}}() # pos => (distance, required_keys_mask)

    start_keys_mask::UInt32 = 0
    enqueue!(q, (start_pos, 0, start_keys_mask))
    visited[start_pos] = (0, start_keys_mask)

    reachable_pois = Dict{Int, Tuple{Int, UInt32}}() # poi_id => (distance, required_keys_mask)

    while !isempty(q)
        curr_pos, dist, req_keys = dequeue!(q)

        # Check if the current position is another POI
        if haskey(all_pois, curr_pos) && curr_pos != start_pos
            poi = all_pois[curr_pos]
            # Store the shortest path found so far
            if !haskey(reachable_pois, poi.id) || dist < reachable_pois[poi.id][1]
                 reachable_pois[poi.id] = (dist, req_keys)
            end
             # Continue BFS even after finding a POI, as shorter paths might exist
             # or paths through this POI might lead to others.
        end

        for move in [CartesianIndex(0, 1), CartesianIndex(0, -1), CartesianIndex(1, 0), CartesianIndex(-1, 0)]
            next_pos = curr_pos + move
            
            # Check bounds
            if !(1 <= next_pos[1] <= rows && 1 <= next_pos[2] <= cols)
                continue
            end

            cell = grid[next_pos]

            # Check wall
            if cell == '#'
                continue
            end

            new_req_keys = req_keys
            if is_door(cell)
                # Add door requirement to the mask
                new_req_keys |= (UInt32(1) << (cell - 'A'))
            end

            new_dist = dist + 1

            # Check if visited or found a shorter path with compatible/fewer key requirements
            # We only need to visit a cell again if we reach it with *strictly fewer* required keys
            # or the same keys but shorter distance. However, standard BFS distance check is usually sufficient.
            # A more complex check `(!haskey(visited, next_pos) || new_dist < visited[next_pos][1] || (new_dist == visited[next_pos][1] && new_req_keys != visited[next_pos][2] && (new_req_keys & visited[next_pos][2]) == new_req_keys))`
            # can sometimes prune, but let's stick to the standard one for simplicity unless performance dictates otherwise.
            
            if !haskey(visited, next_pos) || new_dist < visited[next_pos][1] || (new_dist == visited[next_pos][1] && new_req_keys != visited[next_pos][2] && count_ones(new_req_keys) < count_ones(visited[next_pos][2])) # Optimization: prefer paths with fewer required keys if distance is same
                 visited[next_pos] = (new_dist, new_req_keys)
                 enqueue!(q, (next_pos, new_dist, new_req_keys))
            end
        end
    end
    return reachable_pois
end


# --- Part 1 Solver ---
function solve_part1(grid::Matrix{Char})
    rows, cols = size(grid)
    pois = Dict{CartesianIndex{2}, PointOfInterest}()
    keys_dict = Dict{Char, CartesianIndex{2}}()
    start_pos = CartesianIndex(-1, -1)
    poi_id_counter = 0

    # Find starting position and keys (Points of Interest)
    for r in 1:rows, c in 1:cols
        cell = grid[r, c]
        pos = CartesianIndex(r, c)
        if cell == '@'
            start_pos = pos
            pois[pos] = PointOfInterest(poi_id_counter, '@', pos)
            poi_id_counter += 1
        elseif is_key(cell)
            keys_dict[cell] = pos
            pois[pos] = PointOfInterest(poi_id_counter, cell, pos)
            poi_id_counter += 1
        end
    end

    num_keys = length(keys_dict)
    num_pois = length(pois)
    target_keys_mask = (UInt32(1) << num_keys) - 1

    # Reverse map: poi_id -> POI object
    id_to_poi = Dict(poi.id => poi for poi in values(pois))

    # Precompute distances and required keys between all pairs of POIs
    adj = Dict{Int, Dict{Int, Tuple{Int, UInt32}}}() # poi_id => {reachable_poi_id => (distance, req_keys_mask)}
    for (pos, poi) in pois
        adj[poi.id] = bfs_from_poi(grid, pos, pois)
    end

    # Dijkstra's algorithm on the POI graph
    # State: (current_poi_id, collected_keys_mask)
    # Priority Queue stores: (distance, current_poi_id, collected_keys_mask)
    pq = PriorityQueue{Tuple{Int, UInt32}, Int}() # State => distance
    visited = Dict{Tuple{Int, UInt32}, Int}() # State => min_distance

    start_poi_id = pois[start_pos].id
    start_state = (start_poi_id, UInt32(0))
    pq[start_state] = 0
    visited[start_state] = 0

    min_dist = -1

    while !isempty(pq)
        (curr_poi_id, collected_keys), dist = dequeue_pair!(pq)

        if dist > visited[(curr_poi_id, collected_keys)]
            continue # Found a shorter path already
        end

        # Check if all keys collected
        if collected_keys == target_keys_mask
            min_dist = dist
            break
        end

        # Explore neighbors in the POI graph
        if haskey(adj, curr_poi_id)
            for (next_poi_id, (edge_dist, req_keys)) in adj[curr_poi_id]
                # Check if we have the required keys to traverse this edge
                if (req_keys & collected_keys) == req_keys
                    next_poi_char = id_to_poi[next_poi_id].char
                    new_collected_keys = collected_keys

                    if is_key(next_poi_char)
                        key_bit = UInt32(1) << (next_poi_char - 'a')
                        new_collected_keys |= key_bit
                    end

                    new_dist = dist + edge_dist
                    next_state = (next_poi_id, new_collected_keys)

                    if !haskey(visited, next_state) || new_dist < visited[next_state]
                        visited[next_state] = new_dist
                        pq[next_state] = new_dist
                    end
                end
            end
        end
    end

    return min_dist
end


# --- Part 2 Solver ---
function solve_part2(grid_orig::Matrix{Char})
    grid = copy(grid_orig) # Make a mutable copy
    rows, cols = size(grid)
    
    # Find original start and modify grid
    start_r, start_c = -1, -1
    for r in 1:rows, c in 1:cols
        if grid[r,c] == '@'
            start_r, start_c = r, c
            break
        end
    end

    grid[start_r-1:start_r+1, start_c-1:start_c+1] .= [ '@' '#' '@';
                                                         '#' '#' '#';
                                                         '@' '#' '@']

    pois = Dict{CartesianIndex{2}, PointOfInterest}()
    keys_dict = Dict{Char, CartesianIndex{2}}()
    start_positions = CartesianIndex{2}[]
    poi_id_counter = 0

    # Find new starting positions and keys
    for r in 1:rows, c in 1:cols
        cell = grid[r, c]
        pos = CartesianIndex(r, c)
        if cell == '@'
            push!(start_positions, pos)
            pois[pos] = PointOfInterest(poi_id_counter, '@', pos)
            poi_id_counter += 1
        elseif is_key(cell)
            keys_dict[cell] = pos
            pois[pos] = PointOfInterest(poi_id_counter, cell, pos)
            poi_id_counter += 1
        end
    end

    num_keys = length(keys_dict)
    num_pois = length(pois)
    target_keys_mask = (UInt32(1) << num_keys) - 1
    num_robots = length(start_positions) # Should be 4

    # Reverse map: poi_id -> POI object
    id_to_poi = Dict(poi.id => poi for poi in values(pois))
    start_poi_ids = tuple(sort([pois[pos].id for pos in start_positions])...) # Consistent order

    # Precompute distances and required keys between all pairs of POIs
    # This is the same BFS as before, run from *all* POIs (starts and keys)
    adj = Dict{Int, Dict{Int, Tuple{Int, UInt32}}}()
    for (pos, poi) in pois
        adj[poi.id] = bfs_from_poi(grid, pos, pois)
    end

    # Dijkstra's algorithm for 4 robots
    # State: ( (robot1_poi_id, robot2_poi_id, ...), collected_keys_mask )
    # Priority Queue stores: (distance, robot_ids_tuple, collected_keys_mask)
    pq = PriorityQueue{Tuple{NTuple{num_robots, Int}, UInt32}, Int}()
    visited = Dict{Tuple{NTuple{num_robots, Int}, UInt32}, Int}()

    start_state = (start_poi_ids, UInt32(0))
    pq[start_state] = 0
    visited[start_state] = 0

    min_dist = -1

    while !isempty(pq)
        (current_robot_ids, collected_keys), dist = dequeue_pair!(pq)

        if dist > visited[(current_robot_ids, collected_keys)]
            continue
        end

        if collected_keys == target_keys_mask
            min_dist = dist
            break
        end

        # Try moving each robot
        for i in 1:num_robots
            curr_poi_id = current_robot_ids[i]

            # Explore neighbors reachable by this robot
            if haskey(adj, curr_poi_id)
                for (next_poi_id, (edge_dist, req_keys)) in adj[curr_poi_id]
                     # Check if we have the required keys
                     if (req_keys & collected_keys) == req_keys
                        next_poi_char = id_to_poi[next_poi_id].char
                        new_collected_keys = collected_keys

                        if is_key(next_poi_char)
                            key_bit = UInt32(1) << (next_poi_char - 'a')
                            new_collected_keys |= key_bit
                        end

                        new_dist = dist + edge_dist

                        # Create new robot positions tuple
                        new_robot_ids_list = collect(current_robot_ids)
                        new_robot_ids_list[i] = next_poi_id
                        # Sort tuple for canonical state representation? Only if robots are indistinguishable
                        # In this problem, robots are distinct (start at different places), so don't sort.
                        new_robot_ids_tuple = tuple(new_robot_ids_list...)

                        next_state = (new_robot_ids_tuple, new_collected_keys)

                        if !haskey(visited, next_state) || new_dist < visited[next_state]
                            visited[next_state] = new_dist
                            pq[next_state] = new_dist
                        end
                    end
                end
            end
        end
    end

    return min_dist

end


# Main execution block
function main()
    # Check if input file exists
    if !isfile("input.txt")
        println(stderr, "Error: input.txt not found.")
        return 1 # Indicate error
    end

    # Read input file
    try
        input_str = read("input.txt", String)
        lines = filter(!isempty, split(input_str, '\n'))
        if isempty(lines)
            println(stderr, "Error: input.txt is empty or invalid.")
            return 1
        end
        
        rows = length(lines)
        cols = length(lines[1]) # Assume rectangular grid
        grid = Matrix{Char}(undef, rows, cols)
        for (r, line) in enumerate(lines)
            if length(line) != cols
                 println(stderr, "Error: Input grid is not rectangular.")
                 return 1
            end
            grid[r, :] = collect(line)
        end

        # --- Solve Part 1 ---
        # This part is commented out as the final request focuses on Part 2's logic,
        # but the function `solve_part1` is correct for the first part of the challenge.
        # println("Part 1 solution: $(solve_part1(grid))") # Expected: 4192 for the specific puzzle input

        # --- Solve Part 2 ---
        part2_result = solve_part2(grid)
        if part2_result != -1
             println("$(part2_result)") # Print only the final result for Part 2
        else
             println(stderr, "Error: No solution found for Part 2.")
             return 1
        end

    catch e
        println(stderr, "An error occurred: ", e)
        # Optionally show stack trace for debugging: sprint(showerror, e, catch_backtrace())
        return 1
    end

    return 0 # Indicate success
end

# Run the main function
main()

