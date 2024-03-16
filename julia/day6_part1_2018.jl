using Base.Filesystem

mutable struct Point
    x::Int
    y::Int
end

function main()
    open("input.txt", "r") do file
        points = Point[]
        maxX, maxY = 0, 0
        for line in eachline(file)
            coords = split(line, ", ")
            x, y = parse(Int, coords[1]), parse(Int, coords[2])
            maxX = max(maxX, x)
            maxY = max(maxY, y)
            push!(points, Point(x, y))
        end

        grid = fill(-1, maxX+2, maxY+2)
        areas = zeros(Int, length(points))
        infinite = fill(false, length(points))
        for i in 1:size(grid, 1)
            for j in 1:size(grid, 2)
                minDist = maxX + maxY
                for k in 1:length(points)
                    dist = abs(points[k].x-i) + abs(points[k].y-j)
                    if dist < minDist
                        minDist = dist
                        grid[i, j] = k
                    elseif dist == minDist
                        grid[i, j] = -1
                    end
                end
                if grid[i, j] != -1
                    if i == 1 || j == 1 || i == maxX+2 || j == maxY+2
                        infinite[grid[i, j]] = true
                    end
                    areas[grid[i, j]] += 1
                end
            end
        end

        maxArea = 0
        for i in 1:length(areas)
            if !infinite[i] && areas[i] > maxArea
                maxArea = areas[i]
            end
        end
        println(maxArea)
    end
end

function abs(x::Int)
    return x < 0 ? -x : x
end

main()