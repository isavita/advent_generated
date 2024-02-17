
using DelimitedFiles

const gridSize = 100
const steps = 100

function countOnNeighbors(grid, x, y)
    on = 0
    for dx = -1:1
        for dy = -1:1
            if dx == 0 && dy == 0
                continue
            end
            nx, ny = x + dx, y + dy
            if nx >= 1 && nx <= gridSize && ny >= 1 && ny <= gridSize && grid[nx, ny]
                on += 1
            end
        end
    end
    return on
end

function step(grid)
    newGrid = falses(gridSize, gridSize)
    for x = 1:gridSize
        for y = 1:gridSize
            onNeighbors = countOnNeighbors(grid, x, y)
            if grid[x, y]
                newGrid[x, y] = onNeighbors == 2 || onNeighbors == 3
            else
                newGrid[x, y] = onNeighbors == 3
            end
        end
    end
    return newGrid
end

grid = falses(gridSize, gridSize)
open("input.txt") do file
    y = 1
    for line in eachline(file)
        for (x, c) in enumerate(line)
            grid[x, y] = c == '#'
        end
        y += 1
    end
end

for i = 1:steps
    global grid # Add this line to specify that we are referring to the global variable
    grid = step(grid)
end

onCount = sum(grid)
println(onCount)
