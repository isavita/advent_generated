
const gridSize = 1000

function main()
    grid = zeros(Int, gridSize, gridSize)
    
    file = open("input.txt", "r")
    for instruction in eachline(file)
        processInstruction(instruction, grid)
    end
    close(file)
    
    println(totalBrightness(grid))
end

function processInstruction(instruction, grid)
    parts = split(instruction)
    startX, startY = parse.(Int, split(parts[end-2], ","))
    endX, endY = parse.(Int, split(parts[end], ","))
    
    for x in startX:endX
        for y in startY:endY
            if startswith(instruction, "turn on")
                grid[x, y] += 1
            elseif startswith(instruction, "turn off")
                if grid[x, y] > 0
                    grid[x, y] -= 1
                end
            elseif startswith(instruction, "toggle")
                grid[x, y] += 2
            end
        end
    end
end

function totalBrightness(grid)
    brightness = sum(grid)
    return brightness
end

main()
