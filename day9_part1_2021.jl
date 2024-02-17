using Printf

function main()
    file = open("input.txt", "r")
    heightmap = []
    for line in eachline(file)
        row = [parse(Int, char) for char in line]
        push!(heightmap, row)
    end
    close(file)

    totalRiskLevel = 0
    for y in 1:length(heightmap)
        for x in 1:length(heightmap[y])
            if isLowPoint(heightmap, x, y)
                totalRiskLevel += 1 + heightmap[y][x]
            end
        end
    end

    println(totalRiskLevel)
end

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

main()