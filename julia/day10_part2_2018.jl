function parse_input(filename)
    points = []
    open(filename, "r") do file
        for line in eachline(file)
            match_data = match(r"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>", line)
            if match_data !== nothing
                x, y, vx, vy = match_data.captures
                push!(points, [(parse(Int, x), parse(Int, y)), (parse(Int, vx), parse(Int, vy))])
            end
        end
    end
    return points
end

function simulate_sky(points)
    min_area = Inf
    seconds = 0
    while true
        # Update positions
        for i in eachindex(points)
            points[i][1] = (points[i][1][1] + points[i][2][1], points[i][1][2] + points[i][2][2])
        end

        # Calculate bounding box
        min_x = minimum(p[1][1] for p in points)
        max_x = maximum(p[1][1] for p in points)
        min_y = minimum(p[1][2] for p in points)
        max_y = maximum(p[1][2] for p in points)
        width = max_x - min_x + 1
        height = max_y - min_y + 1
        area = width * height

        # Check if area starts increasing
        if area > min_area
            # Move one step back to state of minimum area
            for i in eachindex(points)
                points[i][1] = (points[i][1][1] - points[i][2][1], points[i][1][2] - points[i][2][2])
            end
            break
        end

        min_area = area
        seconds += 1
    end

    # Print the message
    min_x = minimum(p[1][1] for p in points)
    max_x = maximum(p[1][1] for p in points)
    min_y = minimum(p[1][2] for p in points)
    max_y = maximum(p[1][2] for p in points)

    sky = [copy(fill('.', max_x - min_x + 1)) for _ in min_y:max_y]
    for ((x, y), _) in points
        sky[y - min_y + 1][x - min_x + 1] = '#'
    end

    for line in sky
        println(join(line))
    end

    return seconds
end

function main()
    points = parse_input("input.txt")
    seconds = simulate_sky(points)
    println("Time taken for the message to appear: $seconds seconds")
end

main()