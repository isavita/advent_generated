# Function to parse input and return positions and velocities
function parse_input(filename)
    positions = []
    velocities = []
    open(filename, "r") do file
        for line in eachline(file)
            # Extract position and velocity from the line
            pos_vel = match(r"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>", line)
            push!(positions, (parse(Int, pos_vel[1]), parse(Int, pos_vel[2])))
            push!(velocities, (parse(Int, pos_vel[3]), parse(Int, pos_vel[4])))
        end
    end
    return positions, velocities
end

# Function to simulate the movement of points and detect the message
function simulate_and_detect_message(positions, velocities)
    min_area = Inf
    time = 0
    while true
        # Update positions based on velocities
        for i in 1:length(positions)
            positions[i] = (positions[i][1] + velocities[i][1], positions[i][2] + velocities[i][2])
        end

        # Calculate bounding box area to determine compactness
        xs = [p[1] for p in positions]
        ys = [p[2] for p in positions]
        width = maximum(xs) - minimum(xs)
        height = maximum(ys) - minimum(ys)
        area = width * height

        # Check if area starts increasing
        if area > min_area
            # Move one step back to get the smallest configuration
            for i in 1:length(positions)
                positions[i] = (positions[i][1] - velocities[i][1], positions[i][2] - velocities[i][2])
            end
            break
        end
        min_area = area
        time += 1
    end

    # Print the message
    xs = [p[1] for p in positions]
    ys = [p[2] for p in positions]
    xmin, xmax = minimum(xs), maximum(xs)
    ymin, ymax = minimum(ys), maximum(ys)

    for y in ymin:ymax
        for x in xmin:xmax
            if (x, y) in positions
                print('#')
            else
                print('.')
            end
        end
        println()
    end
end

# Main function to run the program
function main()
    positions, velocities = parse_input("input.txt")
    simulate_and_detect_message(positions, velocities)
end

# Call the main function
main()