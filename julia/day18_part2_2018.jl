const Open = '.'
const Trees = '|'
const Lumberyard = '#'
const Size = 50

function main()
    grid = readInput("input.txt")
    
    seenStates = Dict{String,Int}()
    cycleStart = 0
    cycleLength = 0
    minute = 0
    while true
        state = gridToString(grid)
        if haskey(seenStates, state)
            cycleStart = seenStates[state]
            cycleLength = minute - cycleStart
            break
        end
        seenStates[state] = minute
        grid = transform(grid)
        minute += 1
    end
    
    remainingMinutes = (1000000000 - cycleStart) % cycleLength
    for i in 1:remainingMinutes
        grid = transform(grid)
    end
    
    wooded, lumberyards = countResources(grid)
    println(wooded * lumberyards)
end

function readInput(filename)
    grid = Vector{Vector{Char}}()
    open(filename) do file
        for line in eachline(file)
            row = collect(line)
            push!(grid, row)
        end
    end
    return grid
end

function transform(grid)
    newGrid = [copy(row) for row in grid]
    for i in 1:length(grid)
        for j in 1:length(grid[i])
            newGrid[i][j] = nextAcreState(grid, i, j)
        end
    end
    return newGrid
end

function nextAcreState(grid, i, j)
    if grid[i][j] == Open
        if countAdjacent(grid, i, j, Trees) >= 3
            return Trees
        end
    elseif grid[i][j] == Trees
        if countAdjacent(grid, i, j, Lumberyard) >= 3
            return Lumberyard
        end
    elseif grid[i][j] == Lumberyard
        if countAdjacent(grid, i, j, Lumberyard) >= 1 && countAdjacent(grid, i, j, Trees) >= 1
            return Lumberyard
        end
        return Open
    end
    return grid[i][j]
end

function countAdjacent(grid, i, j, acreType)
    count = 0
    for x in -1:1
        for y in -1:1
            if x == 0 && y == 0
                continue
            end
            if 1 <= i+x <= length(grid) && 1 <= j+y <= length(grid[i]) && grid[i+x][j+y] == acreType
                count += 1
            end
        end
    end
    return count
end

function countResources(grid)
    wooded = 0
    lumberyards = 0
    for row in grid
        for cell in row
            if cell == Trees
                wooded += 1
            elseif cell == Lumberyard
                lumberyards += 1
            end
        end
    end
    return wooded, lumberyards
end

function gridToString(grid)
    return join([join(row) for row in grid], "\n")
end

main()