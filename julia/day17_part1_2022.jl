
function solve_pyroclastic_flow(input_file::String)
    # Read jet pattern from input file
    jet_pattern = strip(read(input_file, String))
    
    # Rock shapes
    rocks = [
        [(0,0), (1,0), (2,0), (3,0)],  # horizontal line
        [(1,0), (0,1), (1,1), (2,1), (1,2)],  # plus sign
        [(0,0), (1,0), (2,0), (2,1), (2,2)],  # L shape
        [(0,0), (0,1), (0,2), (0,3)],  # vertical line
        [(0,0), (1,0), (0,1), (1,1)]   # square
    ]
    
    # Initialize chamber and state
    chamber = Set{Tuple{Int,Int}}()
    jet_index = 1
    max_height = 0
    
    # Simulate rock falling
    for rock_num in 1:2022
        # Select rock shape
        rock = deepcopy(rocks[(rock_num-1) % length(rocks) + 1])
        
        # Position rock at starting location
        rock_x = 2
        rock_y = max_height + 4
        
        # Move rock until it comes to rest
        while true
            # Jet push
            push_dir = jet_pattern[jet_index] == '>' ? 1 : -1
            jet_index = jet_index % length(jet_pattern) + 1
            
            # Try horizontal movement
            can_move_horiz = all((x+rock_x+push_dir, y+rock_y) ∉ chamber && 
                                 0 ≤ x+rock_x+push_dir < 7 
                                 for (x,y) in rock)
            if can_move_horiz
                rock_x += push_dir
            end
            
            # Try downward movement
            can_move_down = all((x+rock_x, y+rock_y-1) ∉ chamber && 
                                y+rock_y-1 > 0 
                                for (x,y) in rock)
            
            if can_move_down
                rock_y -= 1
            else
                # Rock comes to rest
                for (x,y) in rock
                    push!(chamber, (x+rock_x, y+rock_y))
                end
                max_height = max(max_height, maximum(y+rock_y for (x,y) in rock))
                break
            end
        end
    end
    
    return max_height
end

# Read input from file and solve
result = solve_pyroclastic_flow("input.txt")
println("Tower height after 2022 rocks: ", result)
