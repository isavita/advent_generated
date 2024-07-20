
# Function to read the input from a file
function read_input(filename::String)
    return readlines(filename)
end

# Function to count occupied seats based on the new visibility rules
function count_visible_occupied(seats, x, y)
    directions = [
        (-1, -1), (-1, 0), (-1, 1),
        (0, -1),          (0, 1),
        (1, -1), (1, 0), (1, 1)
    ]
    
    occupied_count = 0
    
    for (dx, dy) in directions
        nx, ny = x, y
        while true
            nx += dx
            ny += dy
            
            # Check bounds
            if nx < 1 || nx > size(seats, 1) || ny < 1 || ny > size(seats, 2)
                break
            end
            
            if seats[nx, ny] == '#'
                occupied_count += 1
                break
            elseif seats[nx, ny] == 'L'
                break
            end
        end
    end
    
    return occupied_count
end

# Function to simulate the seating rules
function simulate_seating(seats)
    rows, cols = size(seats)
    changed = true
    
    while changed
        changed = false
        new_seats = copy(seats)
        
        for i in 1:rows
            for j in 1:cols
                if seats[i, j] == 'L' && count_visible_occupied(seats, i, j) == 0
                    new_seats[i, j] = '#'
                    changed = true
                elseif seats[i, j] == '#' && count_visible_occupied(seats, i, j) >= 5
                    new_seats[i, j] = 'L'
                    changed = true
                end
            end
        end
        
        seats = new_seats
    end
    
    return seats
end

# Function to count occupied seats
function count_occupied(seats)
    return count(seats .== '#')
end

# Main function to run the program
function main()
    # Read input from file
    input_lines = read_input("input.txt")
    
    # Convert input to a 2D array
    seats = [collect(line) for line in input_lines]
    seats = hcat(seats...)  # Transpose to get rows and columns correctly
    
    # Simulate seating
    final_seats = simulate_seating(seats)
    
    # Count occupied seats
    occupied_count = count_occupied(final_seats)
    
    # Print the result
    println("Number of occupied seats: ", occupied_count)
end

# Run the main function
main()
