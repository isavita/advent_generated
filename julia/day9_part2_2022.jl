
# Function to read input from a file and return a list of movements
function read_input(filename)
    return readlines(filename)
end

# Function to move the tail based on the head's position
function move_tail(head, tail)
    if abs(head[1] - tail[1]) <= 1 && abs(head[2] - tail[2]) <= 1
        return tail  # No movement needed
    else
        # Move tail towards head
        return (tail[1] + sign(head[1] - tail[1]), tail[2] + sign(head[2] - tail[2]))
    end
end

# Function to simulate the movements of the rope
function simulate_rope(movements, num_knots)
    knots = [(0, 0) for _ in 1:num_knots]  # Initialize all knots at the origin
    visited_positions = Set{Tuple{Int, Int}}()  # Set to track visited positions
    push!(visited_positions, knots[end])  # Add initial position

    for move in movements
        direction, steps = split(move)
        steps = parse(Int, steps)

        for _ in 1:steps
            # Move the head based on the direction
            if direction == "R"
                knots[1] = (knots[1][1] + 1, knots[1][2])
            elseif direction == "L"
                knots[1] = (knots[1][1] - 1, knots[1][2])
            elseif direction == "U"
                knots[1] = (knots[1][1], knots[1][2] + 1)
            elseif direction == "D"
                knots[1] = (knots[1][1], knots[1][2] - 1)
            end

            # Move each subsequent knot
            for i in 2:num_knots
                knots[i] = move_tail(knots[i-1], knots[i])
            end

            # Add the position of the last knot (tail) to the visited set
            push!(visited_positions, knots[end])
        end
    end

    return length(visited_positions)  # Return the number of unique positions visited
end

# Main function to execute the program
function main()
    movements = read_input("input.txt")
    num_knots = 10  # Change this to 2 for part one
    unique_positions = simulate_rope(movements, num_knots)
    println("The tail visited $unique_positions unique positions.")
end

# Run the program
main()
