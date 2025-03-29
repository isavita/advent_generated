
using Printf
using DataStructures # For Queue, although we can implement BFS with a Vector

# --- Data Structures ---

mutable struct Unit
    id::Int
    pos::CartesianIndex{2}
    type::Char # 'E' or 'G'
    hp::Int
    ap::Int
    alive::Bool
end

# Reading order comparison for CartesianIndex
Base.isless(a::CartesianIndex{2}, b::CartesianIndex{2}) = (a[1], a[2]) < (b[1], b[2])

# --- Helper Functions ---

const NEIGHBOR_OFFSETS = [
    CartesianIndex(-1, 0), # Up
    CartesianIndex(0, -1), # Left
    CartesianIndex(0, 1),  # Right
    CartesianIndex(1, 0)   # Down
]

function get_neighbors(pos::CartesianIndex{2}, map_dims::Tuple{Int, Int})
    neighbors = CartesianIndex{2}[]
    for offset in NEIGHBOR_OFFSETS
        neighbor = pos + offset
        if 1 <= neighbor[1] <= map_dims[1] && 1 <= neighbor[2] <= map_dims[2]
            push!(neighbors, neighbor)
        end
    end
    return neighbors
end

function parse_input(filename::String)
    lines = readlines(filename)
    height = length(lines)
    width = length(lines[1])
    map_grid = fill('.', height, width)
    units = Unit[]
    unit_id_counter = 1

    for r in 1:height
        for c in 1:width
            char = lines[r][c]
            map_grid[r, c] = char
            if char == 'E' || char == 'G'
                pos = CartesianIndex(r, c)
                push!(units, Unit(unit_id_counter, pos, char, 200, 3, true))
                unit_id_counter += 1
                # Keep unit marker on the grid for initial state representation
            end
        end
    end
    return map_grid, units
end

# Breadth-First Search to find shortest paths and first steps
# Returns a dictionary: reachable_pos => (distance, first_step_pos)
function bfs(start_pos::CartesianIndex{2}, map_grid::Matrix{Char}, occupied_positions::Set{CartesianIndex{2}})
    map_dims = size(map_grid)
    q = Queue{Tuple{CartesianIndex{2}, Int}}() # (position, distance)
    # Store distance and the *first step* taken from start_pos to reach a position
    # Initialize first_step with a dummy value or the position itself if dist=1
    visited = Dict{CartesianIndex{2}, Tuple{Int, CartesianIndex{2}}}() # pos => (distance, first_step_pos)

    enqueue!(q, (start_pos, 0))
    visited[start_pos] = (0, start_pos) # Distance 0, no step needed

    while !isempty(q)
        current_pos, current_dist = dequeue!(q)

        for neighbor in get_neighbors(current_pos, map_dims)
            # Check map boundaries (already done by get_neighbors), walls, occupied spots, and if already visited
            if map_grid[neighbor] == '.' && !(neighbor in occupied_positions) && !haskey(visited, neighbor)
                 # Determine the first step: If moving from the start, it's the neighbor itself.
                 # Otherwise, inherit the first step from the current_pos path.
                first_step = (current_dist == 0) ? neighbor : visited[current_pos][2]
                visited[neighbor] = (current_dist + 1, first_step)
                enqueue!(q, (neighbor, current_dist + 1))
            end
        end
    end
    return visited # Contains all reachable '.' squares from start_pos
end


# --- Simulation Logic ---

function simulate_combat(map_grid::Matrix{Char}, units::Vector{Unit})
    rounds = 0
    map_dims = size(map_grid)

    while true
        # Sort units by reading order for turn sequence
        sort!(units, by = u -> u.pos)
        
        killed_in_round = Set{Int}() # Track units killed this round

        for i in 1:length(units)
            unit = units[i]

            # Skip if unit died earlier in this round
            if !unit.alive || unit.id in killed_in_round
                continue
            end

            # --- Target Identification ---
            targets = Unit[]
            for other_unit in units
                if other_unit.alive && other_unit.type != unit.type
                     push!(targets, other_unit)
                end
            end

            # Check end condition *before* taking a turn
            if isempty(targets)
                 # Combat ends! Calculate score based on *completed* rounds.
                 total_hp = sum(u.hp for u in units if u.alive)
                 return rounds * total_hp
            end

            occupied_positions = Set(u.pos for u in units if u.alive)

            # --- Check if Already in Range ---
            in_range_targets = Unit[]
            for neighbor in get_neighbors(unit.pos, map_dims)
                 for target in targets
                     if target.pos == neighbor
                         push!(in_range_targets, target)
                         break # Found a target at this neighbor
                     end
                 end
            end
            
            needs_to_move = isempty(in_range_targets)

            # --- Movement Phase ---
            if needs_to_move
                # 1. Identify in-range squares for all targets
                in_range_squares = Set{CartesianIndex{2}}()
                for target in targets
                    for neighbor in get_neighbors(target.pos, map_dims)
                        # Must be open cavern '.' and not occupied currently
                        if map_grid[neighbor] == '.' && !(neighbor in occupied_positions)
                            push!(in_range_squares, neighbor)
                        end
                    end
                end
                
                 # Handle the case where the unit itself occupies a potential target square
                 # (This shouldn't happen if logic is correct, but good to be aware)
                 delete!(in_range_squares, unit.pos) 


                if !isempty(in_range_squares)
                    # 2. Find reachable in-range squares using BFS
                    reachable_info = bfs(unit.pos, map_grid, occupied_positions)

                    # Filter BFS results to only include target squares
                    candidate_destinations = Tuple{Int, CartesianIndex{2}, CartesianIndex{2}}[] # (dist, target_pos, first_step)
                    for target_sq in in_range_squares
                        if haskey(reachable_info, target_sq)
                            dist, first_step = reachable_info[target_sq]
                            push!(candidate_destinations, (dist, target_sq, first_step))
                        end
                    end


                    if !isempty(candidate_destinations)
                        # 3. Choose destination: nearest, then reading order target, then reading order first step
                        min_dist = minimum(c[1] for c in candidate_destinations)
                        nearest_candidates = filter(c -> c[1] == min_dist, candidate_destinations)

                        # Sort by target square reading order
                        sort!(nearest_candidates, by = c -> c[2])
                        chosen_target_sq = nearest_candidates[1][2]

                        # Find all candidates leading to the chosen target square
                        # (Should technically only be one based on BFS properties if implemented carefully,
                        # but let's filter just in case)
                        candidates_for_chosen_target = filter(c -> c[2] == chosen_target_sq, nearest_candidates)

                        # Sort these by the first step's reading order
                        sort!(candidates_for_chosen_target, by = c -> c[3])
                        
                        chosen_first_step = candidates_for_chosen_target[1][3]

                        # 4. Move
                        # Update map (old position becomes '.')
                        map_grid[unit.pos] = '.'
                        # Update unit position
                        unit.pos = chosen_first_step
                        # Update map (new position has unit type) - important for subsequent BFS in the same round
                        map_grid[unit.pos] = unit.type
                        
                        # After moving, re-check for targets in range for potential attack
                         in_range_targets = Unit[]
                         for neighbor in get_neighbors(unit.pos, map_dims)
                            for target in targets # Use the original target list
                                 if target.alive && target.pos == neighbor # Check alive status again
                                     push!(in_range_targets, target)
                                     break 
                                 end
                             end
                         end

                    end # end if !isempty(candidate_destinations)
                end # end if !isempty(in_range_squares)
            end # end if needs_to_move

            # --- Attack Phase ---
             if !isempty(in_range_targets)
                 # Select target: lowest HP, then reading order
                 sort!(in_range_targets, by = t -> (t.hp, t.pos))
                 target_to_attack = in_range_targets[1]

                 # Deal damage
                 target_to_attack.hp -= unit.ap

                 # Check for death
                 if target_to_attack.hp <= 0
                     target_to_attack.alive = false
                     map_grid[target_to_attack.pos] = '.' # Clear map
                     push!(killed_in_round, target_to_attack.id) # Mark as killed this round
                     
                     # Important: Remove the killed unit from occupied positions
                     # This isn't strictly needed if we rebuild occupied_positions each turn,
                     # but good practice if we were optimizing.
                 end
             end # end if attack possible

        end # end loop through units

        # --- End of Round Cleanup ---
        # Remove dead units permanently from the list
        filter!(u -> u.alive, units)
        
        # Check again if combat ended mid-round (e.g., last Goblin killed by second-to-last Elf)
        elf_alive = any(u -> u.type == 'E', units)
        goblin_alive = any(u -> u.type == 'G', units)
        if !elf_alive || !goblin_alive
            # Combat ended during this round, don't increment rounds
             total_hp = sum(u.hp for u in units) # Already filtered dead ones
             return rounds * total_hp
        end

        # If combat continues, increment round counter
        rounds += 1

        # Optional: Print map state for debugging
        # println("\nAfter Round ", rounds)
        # for r in 1:map_dims[1]
        #     row_str = ""
        #     row_hp = String[]
        #     for c in 1:map_dims[2]
        #         row_str *= map_grid[r, c]
        #          found = false
        #          for u in units
        #              if u.pos == CartesianIndex(r,c)
        #                  push!(row_hp, "$(u.type)($(u.hp))")
        #                  found = true
        #                  break
        #              end
        #          end
        #     end
        #     println(row_str, "   ", join(row_hp, ", "))
        # end
        # readline() # Pause between rounds

    end # end while true (main loop)
end

# --- Main Entry Point ---

function main()
    if length(ARGS) > 0 && isfile(ARGS[1])
        input_filename = ARGS[1]
    else
        input_filename = "input.txt"
        if !isfile(input_filename)
            println("Error: Input file '$input_filename' not found.")
            println("Usage: julia script.jl [input_file]")
            return 1
        end
    end

    map_grid, initial_units = parse_input(input_filename)
    
    # Create copies to avoid modifying originals if needed elsewhere
    # Though for this problem, modifying in place is fine.
    current_map = copy(map_grid) 
    current_units = deepcopy(initial_units) 

    outcome = simulate_combat(current_map, current_units)

    println(outcome)
    return 0
end

# Execute main function if the script is run directly
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
