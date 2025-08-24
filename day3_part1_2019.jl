# Function to parse a single wire's path and return all the points it covers
function trace_wire_path(path)
    directions = split(path, ",")
    x, y = 0, 0
    points = Set{Tuple{Int, Int}}()
    for dir in directions
        d = dir[1]
        length = parse(Int, dir[2:end])
        for _ in 1:length
            if d == 'R'
                x += 1
            elseif d == 'L'
                x -= 1
            elseif d == 'U'
                y += 1
            elseif d == 'D'
                y -= 1
            end
            push!(points, (x, y))
        end
    end
    return points
end

# Function to find the Manhattan distance from the central port (0, 0)
manhattan_distance(x, y) = abs(x) + abs(y)

# Main function to read input and calculate the closest intersection distance
function find_closest_intersection(filename)
    lines = readlines(filename)
    wire1_path = lines[1]
    wire2_path = lines[2]

    wire1_points = trace_wire_path(wire1_path)
    wire2_points = trace_wire_path(wire2_path)

    intersections = intersect(wire1_points, wire2_points)
    distances = [manhattan_distance(x, y) for (x, y) in intersections]

    return minimum(distances)
end

# Print the result for the input file
println(find_closest_intersection("input.txt"))