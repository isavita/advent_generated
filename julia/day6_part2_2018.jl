using Printf

# Parse coordinates
function parse_coordinates(input)
    lines = split(strip(input), "\n")
    coordinates = Vector{Tuple{Int,Int}}(undef, length(lines))
    for (i, line) in enumerate(lines)
        coords = parse.(Int, split(line, ", "))
        coordinates[i] = (coords[1], coords[2])
    end
    return coordinates
end

# Find the bounding box
function find_bounding_box(coordinates)
    min_x = minimum(c[1] for c in coordinates)
    min_y = minimum(c[2] for c in coordinates)
    max_x = maximum(c[1] for c in coordinates)
    max_y = maximum(c[2] for c in coordinates)
    return min_x, min_y, max_x, max_y
end

# Calculate Manhattan distance
manhattan_distance(x1, y1, x2, y2) = abs(x1 - x2) + abs(y1 - y2)

# Find the region size
function find_region_size(coordinates, max_distance)
    min_x, min_y, max_x, max_y = find_bounding_box(coordinates)
    region_size = 0
    for x in min_x:max_x
        for y in min_y:max_y
            total_distance = sum(manhattan_distance(x, y, c[1], c[2]) for c in coordinates)
            if total_distance < max_distance
                region_size += 1
            end
        end
    end
    return region_size
end

# Read input from file
input = read("input.txt", String)
coordinates = parse_coordinates(input)
region_size = find_region_size(coordinates, 10000)
@printf("%d\n", region_size)