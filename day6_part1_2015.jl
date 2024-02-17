
const gridSize = 1000

function main()
    grid = falses(gridSize, gridSize)
    
    file = open("input.txt", "r")
    for instruction in eachline(file)
        processInstruction(instruction, grid)
    end
    close(file)
    
    println(countLights(grid))
end

function processInstruction(instruction, grid)
    parts = split(instruction)
    startX, startY = parse.(Int, split(parts[end-2], ","))
    endX, endY = parse.(Int, split(parts[end], ","))
    
    for x in startX[1]:endX[1]
        for y in startY[1]:endY[1]
            if startswith(instruction, "turn on")
                grid[x, y] = true
            elseif startswith(instruction, "turn off")
                grid[x, y] = false
            elseif startswith(instruction, "toggle")
                grid[x, y] = !grid[x, y]
            end
        end
    end
end

function countLights(grid)
    count = 0
    for row in grid
        for light in row
            if light
                count += 1
            end
        end
    end
    return count
end

main()
