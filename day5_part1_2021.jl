
open("input.txt") do file
    grid = Dict{Tuple{Int, Int}, Int}()

    for line in eachline(file)
        startCoords, endCoords = split(line, " -> ")
        startCoords = split(startCoords, ",")
        endCoords = split(endCoords, ",")

        x1, y1 = parse.(Int, startCoords)
        x2, y2 = parse.(Int, endCoords)

        if x1 == x2
            if y1 > y2
                y1, y2 = y2, y1
            end
            for y in y1:y2
                grid[(x1, y)] = get(grid, (x1, y), 0) + 1
            end
        elseif y1 == y2
            if x1 > x2
                x1, x2 = x2, x1
            end
            for x in x1:x2
                grid[(x, y1)] = get(grid, (x, y1), 0) + 1
            end
        end
    end

    overlapCount = count(v -> v > 1, values(grid))
    println(overlapCount)
end
