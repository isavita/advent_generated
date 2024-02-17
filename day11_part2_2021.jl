
function readInput(filename)
    grid = []
    for line in eachline(filename)
        row = [parse(Int, char) for char in line]
        push!(grid, row)
    end
    return grid
end

function simulateStep(grid)
    flashes = 0
    flashed = Dict()

    for y in 1:length(grid)
        for x in 1:length(grid[y])
            grid[y][x] += 1
        end
    end

    for y in 1:length(grid)
        for x in 1:length(grid[y])
            if grid[y][x] > 9
                flashes += flash(grid, x, y, flashed)
            end
        end
    end

    for coords in keys(flashed)
        grid[coords[2]][coords[1]] = 0
    end

    return flashes
end

function flash(grid, x, y, flashed)
    if haskey(flashed, (x, y))
        return 0
    end

    flashed[(x, y)] = true
    flashes = 1
    directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

    for dir in directions
        newX, newY = x + dir[1], y + dir[2]
        if newX >= 1 && newX <= length(grid[1]) && newY >= 1 && newY <= length(grid)
            grid[newY][newX] += 1
            if grid[newY][newX] > 9
                flashes += flash(grid, newX, newY, flashed)
            end
        end
    end

    return flashes
end

grid = readInput("input.txt")
global step = 0
while true
    global step += 1
    flashes = simulateStep(grid)
    if flashes == 100
        break
    end
end

println(step)
