
heightmap = []
open("input.txt") do file
    for line in eachline(file)
        row = [parse(Int, char) for char in line]
        push!(heightmap, row)
    end
end

basinSizes = Int[]
visited = Dict{Tuple{Int, Int}, Bool}()

function isLowPoint(heightmap, x, y)
    height = heightmap[y][x]
    if x > 1 && heightmap[y][x-1] <= height
        return false
    end
    if x < length(heightmap[y]) && heightmap[y][x+1] <= height
        return false
    end
    if y > 1 && heightmap[y-1][x] <= height
        return false
    end
    if y < length(heightmap) && heightmap[y+1][x] <= height
        return false
    end
    return true
end

function exploreBasin(heightmap, x, y, visited)
    if haskey(visited, (x, y)) || heightmap[y][x] == 9
        return 0
    end
    visited[(x, y)] = true
    size = 1

    directions = [(0, -1), (-1, 0), (0, 1), (1, 0)]
    for dir in directions
        newX, newY = x+dir[1], y+dir[2]
        if newX >= 1 && newX <= length(heightmap[1]) && newY >= 1 && newY <= length(heightmap)
            size += exploreBasin(heightmap, newX, newY, visited)
        end
    end
    return size
end

for y in 1:length(heightmap)
    for x in 1:length(heightmap[y])
        if isLowPoint(heightmap, x, y)
            size = exploreBasin(heightmap, x, y, visited)
            push!(basinSizes, size)
        end
    end
end

sort!(basinSizes, rev=true)
result = basinSizes[1] * basinSizes[2] * basinSizes[3]
println(result)
